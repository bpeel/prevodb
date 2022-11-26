/*
 * PReVo - A portable version of ReVo for Android
 * Copyright (C) 2012, 2014, 2016, 2017  Neil Roberts
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; version 2 of the License.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "config.h"

#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <glib/gstdio.h>
#include <glib/gi18n.h>

#include "pdb-db.h"
#include "pdb-lang.h"
#include "pdb-error.h"
#include "pdb-doc.h"
#include "pdb-mkdir.h"
#include "pdb-error.h"
#include "pdb-strcmp.h"
#include "pdb-roman.h"
#include "pdb-list.h"
#include "pdb-file.h"
#include "pdb-span.h"
#include "pdb-trim.h"

/* An article file comprises of up to 16 articles. The articles are
 * bundled together to reduce the number of files because the package
 * installer in older versions of Android seems to have a bug with
 * large numbers of files. We don't want to put them all in one file
 * however because it is not possible to seek within an asset file. */

/* Each article in the file has the following format:
 * • A 4-byte little endian number representing the size of the
 *   article data in bytes
 * • A spanned string representing the title of the article.
 * • A list of sections for the article.
 *
 * Each section consists of the following:
 *
 * • A three-byte string representing the language code for this
 *   section. If the code is shorter than three bytes then the
 *   remaining bytes will be set to zero. The code will usually be
 *   ‘eo’ for esperanto but it can but other codes for the translated
 *   sections.
 * • A spanned string for the section title.
 * • A spanned string with the section contents.
 *
 * A spanned string comprises of:
 * • A two byte little endian number for the length of the string data
 * • The string data in UTF-8
 * • A list of string spans. These comprise of:
 *   • A two-byte length to mark the end of the span
 *   • A two-byte string offset to mark the start of the span
 *   • Two 16-bit numbers of extra data whose meaning depends on the
 *     span type number above.
 *   • One byte as a number representing the intended formatting of the span.
 *   The list of spans is terminated by two zero bytes (which would otherwise
 *   appear as the start of a zero-length span).
 *
 * The length and string offset are counted in 16-bit units as if the
 * string was encoded in UTF-16.
 */

/* If a single output file is selected, the format is as follows:
 * • Three bytes for the magic string ‘PRD’
 * • A one byte file format version number. The current version is 67
 * • Four bytes in little-endian format representing the number of articles.
 * • Four bytes in little-endian for each article representing the
 *   offset in the file to the data for that article.
 * • Four bytes in little-endian representing the number of languages.
 * • A table of offsets to the indices for each language consisting of:
 *   • Four bytes reserved for a null terminated string representing the
 *     language code
 *   • Four bytes in little-endian for the offset to the index for that
 *     language.
 *   The entry for each language is a fixed length which means the table can
 *   be binary chopped.
 * • The data for each language. This consists of:
 *   • A null-terminated string in UTF-8 representing the full language name.
 *   • The data for the index as described in pdb-trie.c
 * • The data for each article in the format described above except for the
 *   following:
 *   • The span offsets and lengths are stored as byte counts rather than
 *     as counts of UTF-16 code points.
 */

/* The first version of the database is 66 because the magic string
 * used to be the four letter code ‘PRDB’. The B has been hijacked as
 * a version number.
 *
 * Subsequent changes to the format are:
 *
 * Version 67:
 * • The language codes for each section were added.
 */

static const char pdb_db_magic[4] = "PRDC";

typedef struct
{
  int length;
  char *text;

  /* A list of PdbSpans */
  PdbList spans;
} PdbDbSpannableString;

typedef struct
{
  int section_num;
  char lang_code[4];

  PdbDbSpannableString title;
  PdbDbSpannableString text;
} PdbDbSection;

typedef struct
{
  int article_num;

  PdbDbSpannableString title;

  /* A list of PdbDbSections */
  GList *sections;
} PdbDbArticle;

/* A reference represents a position in an article. The reference can
 * either directly contain a pointer to the section and article or it
 * can refer to it indirectly via a mark name. These are used in a
 * PdbDbLink to store the location the link refers to and also as the
 * value of the index entries */
typedef enum
{
  PDB_DB_REFERENCE_TYPE_MARK,
  PDB_DB_REFERENCE_TYPE_DIRECT
} PdbDbReferenceType;

typedef struct
{
  PdbDbReferenceType type;

  union
  {
    struct
    {
      PdbDbArticle *article;
      PdbDbSection *section;
    } direct;

    char *mark;
  } d;
} PdbDbReference;

/* A link stores a delayed reference to a section from a reference
 * span. These are all collected in the 'references' list so that they
 * can be resolved later once all of the articles are loaded */
typedef struct
{
  PdbDbSpannableString *string;
  PdbSpan *span;
  PdbDbReference *reference;
} PdbDbLink;

/* PdbDbMark is only used as the value of the 'marks' hash table. This
 * points to a particular secion in an article and is used to convert
 * the mark name to its actual section */
typedef struct
{
  PdbDbArticle *article;
  PdbDbSection *section;
  /* If the mark is in a particular sence then this will be the sence
   * number. Otherwise it will be -1. */
  int sence;
} PdbDbMark;

struct _PdbDb
{
  PdbLang *lang;

  GPtrArray *articles;

  /* Temporary storage location for the word root. This is only valid
   * while parsing an article */
  char *word_root;
  /* Hash table of alternative roots. This will only contain anything
   * while parsing an article. */
  GHashTable *root_variants;

  /* Temporary hash table of the translations indexed by language
   * code. This only contains anything while parsing an article */
  GHashTable *translations;

  GHashTable *marks;

  /* This is a list of links. Each link contains a reference to a
   * section (either directly a pointer or a mark name) and and a
   * pointer to the span. The data in the span will be replaced by an
   * article and mark number as a post-processing step once all of the
   * articles have been read so that the references can be resolved.
   * The links are sorted by the offset */
  GList *links;
};

typedef struct
{
  const char *name;
  const char *replacement;
} PdbDbReplacement;

static const PdbDbReplacement
pdb_db_ref_types[] =
  {
    { "vid", "→" },
    { "hom", "→" },
    { "dif", "=" },
    { "sin", "⇒" },
    { "ant", "⇝" },
    { "super", "↗" },
    { "sub", "↘" },
    { "prt", "↘" },
    { "malprt", "↗" },
    /* { "lst", "??" }, */
    { "ekz", "●" }
  };

static const PdbDbReplacement
pdb_db_styles[] =
  {
    { "KOMUNE", "(komune) " },
    { "FIG", "(figure) " },
    { "ARK", "(arkaismo) " },
    { "EVI", "(evitinde) " },
    { "FRAZ", "(frazaĵo) " },
    { "VULG", "(vulgare) " },
    { "RAR", "(malofte) " },
    { "POE", "(poezie) " },
    { "NEO", "(neologismo) " }
  };

static void
pdb_db_reference_free (PdbDbReference *entry)
{
  switch (entry->type)
    {
    case PDB_DB_REFERENCE_TYPE_MARK:
      g_free (entry->d.mark);
      break;

    case PDB_DB_REFERENCE_TYPE_DIRECT:
      break;
    }

  g_slice_free (PdbDbReference, entry);
}

static PdbDbReference *
pdb_db_reference_copy (const PdbDbReference *entry_in)
{
  PdbDbReference *entry_out = g_slice_dup (PdbDbReference, entry_in);

  switch (entry_in->type)
    {
    case PDB_DB_REFERENCE_TYPE_MARK:
      entry_out->d.mark = g_strdup (entry_out->d.mark);
      break;

    case PDB_DB_REFERENCE_TYPE_DIRECT:
      break;
    }

  return entry_out;
}

static const char *
pdb_db_reference_get_name (const PdbDbReference *ref)
{
  switch (ref->type)
    {
    case PDB_DB_REFERENCE_TYPE_MARK:
      return ref->d.mark;

    case PDB_DB_REFERENCE_TYPE_DIRECT:
      return ref->d.direct.section->title.text;
    }

  g_assert_not_reached ();
}

static void
pdb_db_link_free (PdbDbLink *link)
{
  pdb_db_reference_free (link->reference);
  g_slice_free (PdbDbLink, link);
}

static void
pdb_db_add_index_entry (PdbDb *db,
                        const char *lang,
                        const char *name,
                        const char *display_name,
                        const PdbDbReference *entry_in)
{
  PdbTrie *trie;

  if (*name == '\0')
    {
      fprintf (stderr,
               _("empty name found for index entry \"%s\" "
                 "in language \"%s\"\n"),
               pdb_db_reference_get_name (entry_in),
               lang);
      return;
    }

  if (display_name && *display_name == '\0')
    {
      fprintf (stderr,
               _("empty display name found for index entry "
                 "\"%s\" in language \"%s\"\n"),
               pdb_db_reference_get_name (entry_in),
               lang);
      return;
    }

  trie = pdb_lang_get_trie (db->lang, lang);

  if (trie)
    {
      PdbDbReference *entry =
        pdb_db_reference_copy (entry_in);
      const char *p;

      /* Check if any of the characters in the name are upper case */
      for (p = name; *p; p = g_utf8_next_char (p))
        {
          gunichar ch = g_utf8_get_char (p);

          if (g_unichar_isupper (ch))
            break;
        }

      /* If we found an uppercase character then we'll additionally
       * add a lower case representation of the name so that the
       * search can be case insensitive */
      if (*p || display_name)
        {
          GString *buf = g_string_new (NULL);

          for (p = name; *p; p = g_utf8_next_char (p))
            {
              gunichar ch = g_unichar_tolower (g_utf8_get_char (p));
              g_string_append_unichar (buf, ch);
            }

          pdb_trie_add_word (trie,
                             buf->str,
                             display_name ? display_name : name,
                             entry);

          g_string_free (buf, TRUE);
        }
      else
        {
          pdb_trie_add_word (trie,
                             name,
                             NULL,
                             entry);
        }
    }
}

static void
pdb_db_append_tld (PdbDb *db,
                   GString *buf,
                   char **atts)
{
  const char *root = db->word_root;
  const char *lit = NULL;
  const char *var;
  char **att;

  for (att = atts; att[0]; att += 2)
    {
      if (!strcmp (att[0], "lit"))
        {
          lit = att[1];
        }
      else if (!strcmp (att[0], "var"))
        {
          var = g_hash_table_lookup (db->root_variants, att[1]);

          if (var == NULL)
            {
              fprintf (stderr,
                       _("missing variant \"%s\" for root \"%s\"\n"),
                       att[1],
                       db->word_root);
            }
          else
            {
              root = var;
            }
        }
    }

  if (lit)
    {
      g_string_append (buf, lit);

      if (*root)
        root = g_utf8_next_char (root);
    }

  g_string_append (buf, root);
}

static void
pdb_db_mark_free (PdbDbMark *mark)
{
  g_slice_free (PdbDbMark, mark);
}

static void
pdb_db_destroy_spannable_string (PdbDbSpannableString *string)
{
  g_free (string->text);
  pdb_span_free_list (&string->spans);
}

static void
pdb_db_free_section_cb (void *ptr,
                        void *user_data)
{
  PdbDbSection *section = ptr;
  pdb_db_destroy_spannable_string (&section->title);
  pdb_db_destroy_spannable_string (&section->text);
  g_slice_free (PdbDbSection, section);
}

static void
pdb_db_free_section_list (GList *sections)
{
  g_list_foreach (sections, pdb_db_free_section_cb, NULL);
  g_list_free (sections);
}

static int
pdb_db_get_element_num (PdbDocElementNode *element)
{
  PdbDocNode *n;
  int num = 0;

  /* Count the matching elements before this one */
  for (n = element->node.prev; n; n = n->prev)
    if (n->type == PDB_DOC_NODE_TYPE_ELEMENT &&
        !strcmp (((PdbDocElementNode *) n)->name, element->name))
      num++;

  /* If this is the first matching element then check if it's also the
   * only one */
  if (num == 0)
    {
      for (n = element->node.next; n; n = n->next)
        if (n->type == PDB_DOC_NODE_TYPE_ELEMENT &&
            !strcmp (((PdbDocElementNode *) n)->name, element->name))
          /* It's not the only one so return a real number */
          return 0;

      /* It is the only one so return -1 */
      return -1;
    }
  else
    return num;
}

static PdbDbMark *
pdb_db_add_mark (PdbDb *db,
                 PdbDbArticle *article,
                 PdbDbSection *section,
                 const char *mark_name)
{
  PdbDbMark *mark = g_slice_new (PdbDbMark);

  mark->article = article;
  mark->section = section;
  mark->sence = -1;

  g_hash_table_insert (db->marks,
                       g_strdup (mark_name),
                       mark);

  return mark;
}

static int
pdb_db_get_sence_number (const PdbDocElementNode *elem)
{
  /* Look for a containing “snc” element */
  while (elem && elem->node.type == PDB_DOC_NODE_TYPE_ELEMENT)
    {
      if (!strcmp (elem->name, "snc"))
        {
          int sence_num = 1;

          /* Count the previous sences */
          for (PdbDocNode *node = elem->node.prev; node; node = node->prev)
            {
              if (node->type == PDB_DOC_NODE_TYPE_ELEMENT &&
                  !strcmp (((PdbDocElementNode *) node)->name, "snc"))
                sence_num++;
            }

          return sence_num;
        }

      elem = (PdbDocElementNode *) elem->node.parent;
    }

  return -1;
}

static void
pdb_db_add_mark_from_element_to_reference (PdbDb *db,
                                           const PdbDbReference *reference,
                                           PdbDocElementNode *elem)
{
  /* Only add marks to direct references */
  if (reference->type != PDB_DB_REFERENCE_TYPE_DIRECT)
    return;

  const char *mrk = pdb_doc_get_attribute (elem, "mrk");

  if (mrk == NULL)
    return;

  PdbDbMark *mark = pdb_db_add_mark (db,
                                     reference->d.direct.article,
                                     reference->d.direct.section,
                                     mrk);

  mark->sence = pdb_db_get_sence_number (elem);
}

typedef struct
{
  GString *buf;
  PdbList spans;
} PdbDbTranslationData;

static void
pdb_db_free_translation_data_cb (void *user_data)
{
  PdbDbTranslationData *data = user_data;

  if (data->buf)
    g_string_free (data->buf, TRUE);

  pdb_span_free_list (&data->spans);

  g_slice_free (PdbDbTranslationData, data);
}

static int
pdb_db_compare_language_code (const void *a,
                              const void *b,
                              void *user_data)
{
  PdbDb *db = user_data;
  const char *code_a = a;
  const char *code_b = b;
  const char *name_a, *name_b;

  name_a = pdb_lang_get_name (db->lang, code_a);
  if (name_a == NULL)
    name_a = code_a;

  name_b = pdb_lang_get_name (db->lang, code_b);
  if (name_b == NULL)
    name_b = code_b;

  return pdb_strcmp (name_a, name_b);
}

static void
trim_trailing_commas (GString *buf)
{
  gsize len = buf->len;

  while (len > 0 &&
         (g_ascii_isspace (buf->str[len - 1]) ||
          buf->str[len - 1] == ','))
    len--;

  g_string_truncate (buf, len);
}

static gboolean
pdb_db_get_trd_link (PdbDb *db,
                     PdbDocElementNode *trd_elem,
                     const PdbDbReference *reference,
                     GString *buf,
                     PdbList *spans,
                     GError **error)
{
  PdbDocElementNode *parent, *kap;
  PdbDocNode *n;
  int sence_num = -1;
  int subsence_num = -1;
  PdbSpan *span;
  PdbDbLink *link;
  int span_start, span_end;

  /* Check that the parent is either <snc> or <subsnc> */
  parent = (PdbDocElementNode *) trd_elem->node.parent;

  if (!strcmp (parent->name, "dif"))
    parent = (PdbDocElementNode *) parent->node.parent;

  if (!strcmp (parent->name, "subsnc"))
    {
      subsence_num = pdb_db_get_element_num (parent);
      parent = (PdbDocElementNode *) parent->node.parent;
    }

  if (!strcmp (parent->name, "snc"))
    {
      sence_num = pdb_db_get_element_num (parent);
      parent = (PdbDocElementNode *) parent->node.parent;
    }

  if (!strcmp (parent->name, "subdrv") ||
      !strcmp (parent->name, "subart"))
    parent = (PdbDocElementNode *) parent->node.parent;

  if (strcmp (parent->name, "drv") &&
      strcmp (parent->name, "art"))
    {
      g_set_error (error,
                   PDB_ERROR,
                   PDB_ERROR_BAD_FORMAT,
                   _("%s tag found with unknown parent %s"),
                   trd_elem->name,
                   parent->name);
      return FALSE;
    }

  kap = pdb_doc_get_child_element (&parent->node, "kap");

  if (kap == NULL)
    {
      g_set_error (error,
                   PDB_ERROR,
                   PDB_ERROR_BAD_FORMAT,
                   _("drv node found without a kap"));
      return FALSE;
    }

  span_start = buf->len;

  for (n = kap->node.first_child; n; n = n->next)
    {
      switch (n->type)
        {
        case PDB_DOC_NODE_TYPE_TEXT:
          {
            PdbDocTextNode *text = (PdbDocTextNode *) n;
            const char *p, *end;

            for (p = text->data, end = text->data + text->len;
                 p < end;
                 p++)
              if (g_ascii_isspace (*p))
                {
                  if (buf->len > 0 &&
                      !g_ascii_isspace (buf->str[buf->len - 1]))
                    g_string_append_c (buf, ' ');
                }
              else
                g_string_append_c (buf, *p);
          }
          break;

        case PDB_DOC_NODE_TYPE_ELEMENT:
          {
            PdbDocElementNode *elem = (PdbDocElementNode *) n;

            if (!strcmp (elem->name, "tld") ||
                !strcmp (elem->name, "rad"))
              g_string_append_c (buf, '~');
          }
          break;
        }
    }

  /* If the root has variants then the text of the kap element will
   * have trailing commas */
  trim_trailing_commas (buf);

  if (sence_num != -1)
    {
      g_string_append_printf (buf, " %i", sence_num + 1);

      if (subsence_num != -1)
        g_string_append_printf (buf, ".%c", subsence_num + 'a');
    }

  span_end = buf->len;

  span = g_slice_new0 (PdbSpan);
  span->span_length = span_end - span_start;
  span->span_start = span_start;
  span->type = PDB_SPAN_REFERENCE;
  /* Add this span to the end of the list */
  pdb_list_insert (spans->prev, &span->link);

  link = g_slice_new (PdbDbLink);
  link->span = span;
  link->reference = pdb_db_reference_copy (reference);
  link->string = NULL;

  db->links = g_list_prepend (db->links, link);

  return TRUE;
}

static gboolean
pdb_db_is_empty_translation (PdbDocElementNode *element)
{
  PdbDocNode *node;

  /* Some of the documents (eg, 'missupozi' for French) have empty
   * translations. We want to totally skip these so that they don't
   * mess up the index */

  for (node = element->node.first_child; node; node = node->next)
    switch (node->type)
      {
      case PDB_DOC_NODE_TYPE_TEXT:
        {
          PdbDocTextNode *text = (PdbDocTextNode *) node;
          const char *end = text->data + text->len;
          const char *p;

          for (p = text->data; p < end; p++)
            if (!g_ascii_isspace (*p))
              return FALSE;
        }
        break;

      case PDB_DOC_NODE_TYPE_ELEMENT:
        if (!pdb_db_is_empty_translation ((PdbDocElementNode *) node))
          return FALSE;
      }

  return TRUE;
}

static gboolean
pdb_db_add_trd_index (PdbDb *db,
                      PdbDocElementNode *element,
                      const char *lang_code,
                      const PdbDbReference *reference,
                      GError **error)
{
  PdbDocElementNode *ind;
  GString *display_name;

  display_name = g_string_new (NULL);

  pdb_doc_append_element_text_with_ignore (element,
                                           display_name,
                                           "ofc",
                                           NULL);
  pdb_trim_buf (display_name);

  if ((ind = pdb_doc_get_child_element (&element->node, "ind")))
    {
      GString *real_name = g_string_new (NULL);

      pdb_doc_append_element_text (ind, real_name);
      pdb_trim_buf (real_name);

      pdb_db_add_index_entry (db,
                              lang_code,
                              real_name->str,
                              display_name->str,
                              reference);

      g_string_free (real_name, TRUE);
    }
  else if (pdb_doc_element_has_child_element (element))
    {
      GString *real_name = g_string_new (NULL);

      /* We don't want <klr> tags in the index name */
      pdb_doc_append_element_text_with_ignore (element,
                                               real_name,
                                               "ofc",
                                               "klr",
                                               NULL);
      pdb_trim_buf (real_name);

      pdb_db_add_index_entry (db,
                              lang_code,
                              real_name->str,
                              display_name->str,
                              reference);

      g_string_free (real_name, TRUE);
    }
  else
    {
      pdb_db_add_index_entry (db,
                              lang_code,
                              display_name->str,
                              NULL,
                              reference);
    }

  g_string_free (display_name, TRUE);

  return TRUE;
}

static gboolean
pdb_db_add_translation_index (PdbDb *db,
                              PdbDocElementNode *element,
                              const PdbDbReference *reference,
                              GError **error)
{
  const char *lang_code;

  lang_code = pdb_doc_get_attribute (element, "lng");

  if (lang_code == NULL)
    {
      g_set_error (error,
                   PDB_ERROR,
                   PDB_ERROR_BAD_FORMAT,
                   _("%s element with no lng attribute"),
                   element->name);
      return FALSE;
    }

  if (!strcmp (element->name, "trdgrp"))
    {
      PdbDocNode *node;

      for (node = element->node.first_child; node; node = node->next)
        if (node->type == PDB_DOC_NODE_TYPE_ELEMENT &&
            !pdb_db_add_trd_index (db,
                                   (PdbDocElementNode *) node,
                                   lang_code,
                                   reference,
                                   error))
          return FALSE;

      return TRUE;
    }
  else
    return pdb_db_add_trd_index (db, element, lang_code, reference, error);
}

static gboolean
pdb_db_handle_translation (PdbDb *db,
                           PdbDocElementNode *element,
                           const PdbDbReference *reference,
                           GError **error)
{
  const char *lang_code;
  PdbDbTranslationData *data;
  GString *content;

  /* Silently ignore empty translations */
  if (pdb_db_is_empty_translation (element))
    return TRUE;

  lang_code = pdb_doc_get_attribute (element, "lng");

  if (lang_code == NULL)
    {
      g_set_error (error,
                   PDB_ERROR,
                   PDB_ERROR_BAD_FORMAT,
                   _("%s element with no lng attribute"),
                   element->name);
      return FALSE;
    }
  else if (strlen (lang_code) > 3)
    {
      g_set_error (error,
                   PDB_ERROR,
                   PDB_ERROR_BAD_FORMAT,
                   _("The language code “%s” is too long"),
                   lang_code);
      return FALSE;
    }

  if ((data = g_hash_table_lookup (db->translations,
                                   lang_code)) == NULL)
    {
      data = g_slice_new (PdbDbTranslationData);
      data->buf = g_string_new (NULL);
      pdb_list_init (&data->spans);
      g_hash_table_insert (db->translations,
                           g_strdup (lang_code),
                           data);
    }
  else if (data->buf->len > 0)
    g_string_append (data->buf, "; ");

  if (!pdb_db_get_trd_link (db,
                            element,
                            reference,
                            data->buf,
                            &data->spans,
                            error))
    return FALSE;

  g_string_append (data->buf, ": ");

  content = g_string_new (NULL);
  pdb_doc_append_element_text (element, content);
  pdb_trim_buf (content);
  g_string_append_len (data->buf, content->str, content->len);
  g_string_free (content, TRUE);

  return pdb_db_add_translation_index (db,
                                       element,
                                       reference,
                                       error);
}

static gboolean
pdb_db_find_translations_recursive_skip (PdbDb *db,
                                         PdbDocElementNode *root_node,
                                         const char *skip,
                                         const PdbDbReference *reference,
                                         GError **error)
{
  GPtrArray *stack;
  gboolean ret = TRUE;

  pdb_db_add_mark_from_element_to_reference (db, reference, root_node);

  stack = g_ptr_array_new ();

  if (root_node->node.first_child)
    g_ptr_array_add (stack, root_node->node.first_child);

  while (stack->len > 0)
    {
      PdbDocNode *node = g_ptr_array_index (stack, stack->len - 1);
      g_ptr_array_set_size (stack, stack->len - 1);

      if (node->next)
        g_ptr_array_add (stack, node->next);

      if (node->type == PDB_DOC_NODE_TYPE_ELEMENT)
        {
          PdbDocElementNode *element = (PdbDocElementNode *) node;

          if (!strcmp (element->name, "trdgrp") ||
              !strcmp (element->name, "trd"))
            {
              if (!pdb_db_handle_translation (db,
                                              element,
                                              reference,
                                              error))
                {
                  ret = FALSE;
                  break;
                }
            }
          else if (node->first_child &&
                   strcmp (element->name, "ekz") &&
                   strcmp (element->name, "bld") &&
                   strcmp (element->name, "adm") &&
                   strcmp (element->name, "fnt") &&
                   (skip == NULL || strcmp (element->name, skip)))
            g_ptr_array_add (stack, node->first_child);

          pdb_db_add_mark_from_element_to_reference (db, reference, element);
        }
    }

  g_ptr_array_free (stack, TRUE);

  return ret;
}

static gboolean
pdb_db_find_translations_recursive (PdbDb *db,
                                    PdbDocElementNode *root_node,
                                    const PdbDbReference *reference,
                                    GError **error)
{
  return pdb_db_find_translations_recursive_skip (db,
                                                  root_node,
                                                  NULL, /* skip */
                                                  reference,
                                                  error);
}

static gboolean
pdb_db_find_translations (PdbDb *db,
                          PdbDocElementNode *root_node,
                          const PdbDbReference *reference,
                          GError **error)
{
  PdbDocNode *node;
  gboolean ret = TRUE;

  pdb_db_add_mark_from_element_to_reference (db, reference, root_node);

  for (node = root_node->node.first_child; node; node = node->next)
    if (node->type == PDB_DOC_NODE_TYPE_ELEMENT)
      {
        PdbDocElementNode *element = (PdbDocElementNode *) node;

        if (!strcmp (element->name, "trdgrp") ||
            !strcmp (element->name, "trd"))
          {
            if (!pdb_db_handle_translation (db,
                                            element,
                                            reference,
                                            error))
              {
                ret = FALSE;
                break;
              }
          }
      }

  return ret;
}

static PdbDbSection *
pdb_db_section_new (const char *lang_code)
{
  PdbDbSection *section = g_slice_new (PdbDbSection);
  int code_len = strlen (lang_code);

  memcpy (section->lang_code, lang_code, code_len);
  memset (section->lang_code + code_len, 0, 4 - code_len);

  return section;
}

static GList *
pdb_db_flush_translations (PdbDb *db)
{
  GQueue sections;
  GList *keys, *l;

  g_queue_init (&sections);

  keys = g_hash_table_get_keys (db->translations);

  keys = g_list_sort_with_data (keys,
                                pdb_db_compare_language_code,
                                db);
  for (l = keys; l; l = l->next)
    {
      const char *lang_code = l->data;
      const char *lang_name = pdb_lang_get_name (db->lang, lang_code);
      PdbDbTranslationData *data =
        g_hash_table_lookup (db->translations, lang_code);
      PdbDbSection *section = pdb_db_section_new (lang_code);

      if (lang_name == NULL)
        lang_name = lang_code;

      section->title.length = strlen (lang_name);
      section->title.text = g_strdup (lang_name);
      pdb_list_init (&section->title.spans);

      section->text.length = data->buf->len;
      section->text.text = g_string_free (data->buf, FALSE);
      pdb_list_init (&section->text.spans);
      pdb_list_insert_list (section->text.spans.prev, &data->spans);

      data->buf = NULL;
      pdb_list_init (&data->spans);

      g_queue_push_tail (&sections, section);
    }

  g_list_free (keys);

  g_hash_table_remove_all (db->translations);

  return sections.head;
}

static gboolean
pdb_db_resolve_reference (PdbDb *db,
                          const PdbDbReference *ref,
                          int *article_num,
                          int *section_num,
                          int *sence_num)
{
  gboolean ret = TRUE;

  switch (ref->type)
    {
    case PDB_DB_REFERENCE_TYPE_MARK:
      {
        PdbDbMark *mark =
          g_hash_table_lookup (db->marks, ref->d.mark);

        if (mark == NULL)
          {
            /* Try the mark again with increasingly less precision
             * until will find one that matches so that we can at
             * least point to the right article */
            char *mark_copy = g_strdup (ref->d.mark);
            char *dot;

            while ((dot = strrchr (mark_copy, '.')))
              {
                *dot = '\0';

                if ((mark = g_hash_table_lookup (db->marks, mark_copy)))
                  {
                    fprintf (stderr,
                             _("using less precise mark \"%s\" for \"%s\"\n"),
                             mark_copy,
                             ref->d.mark);
                    break;
                  }
              }

            g_free (mark_copy);
          }

        if (mark)
          {
            *article_num = mark->article->article_num;
            *section_num = mark->section->section_num;
            *sence_num = mark->sence;
          }
        else
          {
            *article_num = 0;
            *section_num = 0;
            *sence_num = -1;
            fprintf (stderr,
                     _("no mark found for reference \"%s\"\n"),
                     ref->d.mark);
            ret = FALSE;
          }
      }
      break;

    case PDB_DB_REFERENCE_TYPE_DIRECT:
      {
        *article_num = ref->d.direct.article->article_num;
        *section_num = ref->d.direct.section->section_num;
        *sence_num = -1;
      }
      break;
    }

  return ret;
}

static void
pdb_db_set_sncref (PdbDb *db,
                   PdbDbLink *link,
                   int article_num,
                   int section_num,
                   int sence_num)
{
  g_assert (article_num < db->articles->len);

  PdbDbArticle *article = db->articles->pdata[article_num];
  PdbDbSection *section = g_list_nth_data (article->sections, section_num);

  g_assert (section != NULL);

  if (sence_num < 0 || link->string == NULL)
    {
      if (link->reference->type == PDB_DB_REFERENCE_TYPE_MARK)
        {
          fprintf (stderr,
                   _("sncref “%s” doesn’t point to a snc\n"),
                   link->reference->d.mark);
        }
      else
        {
          fprintf (stderr,
                   _("reference to section with title “%.*s” "
                     "contains an invalid sncref\n"),
                   section->title.length,
                   section->title.text);
        }
      pdb_list_remove (&link->span->link);
      pdb_span_free (link->span);
      return;
    }

  char buf[16];
  snprintf (buf, sizeof buf, "%i", sence_num);
  int len = strlen (buf);

  PdbDbSpannableString *string = link->string;
  PdbSpan *span = link->span;
  char *rep_text = g_malloc (string->length + len + 1);
  memcpy (rep_text, string->text, span->span_start);
  memcpy (rep_text + span->span_start, buf, len);
  memcpy (rep_text + span->span_start + len,
          string->text + span->span_start,
          string->length - span->span_start);
  rep_text[string->length + len] = '\0';
  g_free (string->text);
  string->text = rep_text;
  string->length += len;
  span->span_length += len;

  PdbSpan *s;

  pdb_list_for_each (s, &string->spans, link)
    {
      if (s == span)
        continue;

      if (s->span_start > span->span_start)
        s->span_start += len;
    }
}

static void
pdb_db_resolve_links (PdbDb *db)
{
  int article_num;
  GList *l;

  /* Calculate the article and section numbers */
  for (article_num = 0; article_num < db->articles->len; article_num++)
    {
      PdbDbArticle *article = g_ptr_array_index (db->articles, article_num);
      int section_num;

      article->article_num = article_num;
      for (section_num = 0, l = article->sections;
           l;
           section_num++, l = l->next)
        {
          PdbDbSection *section = l->data;
          section->section_num = section_num;
        }
    }

  /* Resolve all of the links */
  for (l = db->links; l; l = l->next)
    {
      PdbDbLink *link = l->data;
      int article_num, section_num, sence_num;

      if (pdb_db_resolve_reference (db,
                                    link->reference,
                                    &article_num,
                                    &section_num,
                                    &sence_num))
        {
          if (link->span->type == PDB_SPAN_SUPERSCRIPT)
            {
              pdb_db_set_sncref (db,
                                 link,
                                 article_num,
                                 section_num,
                                 sence_num);
            }
          else
            {
              link->span->data1 = article_num;
              link->span->data2 = section_num;
            }
        }
      else
        {
          /* Remove the span if we couldn't resolve the reference so
           * that it won't leave a link pointing to the wrong
           * article */
          pdb_list_remove (&link->span->link);
          pdb_span_free (link->span);
        }
    }
}

typedef enum
{
  PDB_DB_STACK_NODE,
  PDB_DB_STACK_CLOSE_SPAN,
  PDB_DB_STACK_ADD_PARAGRAPH,
  PDB_DB_STACK_CLOSING_CHARACTER
} PdbDbStackType;

typedef struct
{
  PdbDbStackType type;

  union
  {
    PdbDocNode *node;
    PdbSpan *span;
    gunichar character;
  } d;
} PdbDbParseStackEntry;

typedef enum
{
    PDB_DB_QUEUED_SPACE_TYPE_NONE,
    PDB_DB_QUEUED_SPACE_TYPE_SPACE,
    PDB_DB_QUEUED_SPACE_TYPE_PARAGRAPH,
} PdbDbQueuedSpaceType;

typedef struct
{
  GArray *stack;
  GString *buf;
  PdbDbSpannableString *string;
  PdbList spans;
  PdbDbQueuedSpaceType queued_space;
  gboolean skip_children;
} PdbDbParseState;

static PdbDbParseStackEntry *
pdb_db_parse_push_entry (PdbDbParseState *state,
                         PdbDbStackType type)
{
  PdbDbParseStackEntry *entry;

  g_array_set_size (state->stack, state->stack->len + 1);
  entry = &g_array_index (state->stack,
                          PdbDbParseStackEntry,
                          state->stack->len - 1);

  entry->type = type;

  return entry;
}

static PdbDbParseStackEntry *
pdb_db_parse_push_node (PdbDbParseState *state,
                        PdbDocNode *node)
{
  PdbDbParseStackEntry *entry =
    pdb_db_parse_push_entry (state, PDB_DB_STACK_NODE);

  entry->d.node = node;

  return entry;
}

static PdbDbParseStackEntry *
pdb_db_parse_push_add_paragraph (PdbDbParseState *state)
{
  return pdb_db_parse_push_entry (state, PDB_DB_STACK_ADD_PARAGRAPH);
}

static PdbDbParseStackEntry *
pdb_db_parse_push_closing_character (PdbDbParseState *state,
                                     gunichar ch)
{
  PdbDbParseStackEntry *entry =
    pdb_db_parse_push_entry (state, PDB_DB_STACK_CLOSING_CHARACTER);

  entry->d.character = ch;

  return entry;
}

typedef gboolean (* PdbDbElementHandler) (PdbDb *db,
                                          PdbDbParseState *state,
                                          PdbDocElementNode *element,
                                          PdbSpan *span,
                                          GError **error);

typedef struct
{
  const char *name;
  PdbSpanType type;
  PdbDbElementHandler handler;
  gboolean paragraph;
} PdbDbElementSpan;

static void
pdb_db_start_text (PdbDbParseState *state)
{
  if (state->buf->len > 0)
    {
      switch (state->queued_space)
        {
        case PDB_DB_QUEUED_SPACE_TYPE_NONE:
          break;

        case PDB_DB_QUEUED_SPACE_TYPE_SPACE:
          /* We can get doubled-up spaces if the space was flushed
           * because of the start of a span and the new span also
           * begins with a space.
           */
          if (state->buf->str[state->buf->len - 1] != ' ' &&
              state->buf->str[state->buf->len - 1] != '\n')
            g_string_append_c (state->buf, ' ');
          break;

        case PDB_DB_QUEUED_SPACE_TYPE_PARAGRAPH:
          g_string_append (state->buf, "\n\n");
          break;
        }
    }

  state->queued_space = PDB_DB_QUEUED_SPACE_TYPE_NONE;
}

static PdbSpan *
pdb_db_start_span (PdbDbParseState *state,
                   PdbSpanType type)
{
  PdbSpan *span = g_slice_new0 (PdbSpan);
  PdbDbParseStackEntry *entry;

  span->span_start = state->buf->len;
  span->type = type;

  /* Add the span to the end of the list */
  pdb_list_insert (state->spans.prev, &span->link);
  /* Push the span onto the state.stack so that we can
   * fill in the span length once all of the child
   * nodes have been processed */
  entry = pdb_db_parse_push_entry (state, PDB_DB_STACK_CLOSE_SPAN);
  entry->d.span = span;

  return span;
}

static gboolean
pdb_db_handle_aut (PdbDb *db,
                   PdbDbParseState *state,
                   PdbDocElementNode *element,
                   PdbSpan *span,
                   GError **error)
{
  pdb_db_start_text (state);
  g_string_append (state->buf, "[");
  pdb_db_parse_push_closing_character (state, ']');

  return TRUE;
}

static gboolean
pdb_db_handle_rim (PdbDb *db,
                   PdbDbParseState *state,
                   PdbDocElementNode *element,
                   PdbSpan *span,
                   GError **error)
{
  PdbSpan *bold_span = g_slice_new0 (PdbSpan);

  pdb_db_start_text (state);

  bold_span->type = PDB_SPAN_BOLD;
  bold_span->span_start = state->buf->len;

  g_string_append (state->buf, "Rim. ");

  bold_span->span_length = (state->buf->len -
                            bold_span->span_start);
  pdb_list_insert (state->spans.prev, &bold_span->link);

  return TRUE;
}

static gboolean
pdb_db_add_replacement (PdbDbParseState *state,
                        const char *name,
                        const PdbDbReplacement *replacements,
                        int n_replacements)
{
  int i;

  for (i = 0; i < n_replacements; i++)
    {
      const PdbDbReplacement *replacement = replacements + i;

      if (!strcmp (name, replacement->name))
        {
          pdb_db_start_text (state);
          g_string_append (state->buf, replacement->replacement);

          return TRUE;
        }
    }

  return FALSE;
}

static gboolean
pdb_db_add_replacement_contents (PdbDbParseState *state,
                                 PdbDocElementNode *element,
                                 const PdbDbReplacement *replacements,
                                 int n_replacements)
{
  GString *buf = g_string_new (NULL);
  gboolean ret;

  pdb_doc_append_element_text (element, buf);
  pdb_trim_buf (buf);
  ret = pdb_db_add_replacement (state, buf->str, replacements, n_replacements);
  g_string_free (buf, TRUE);

  return ret;
}

static void
pdb_db_handle_reference_type (PdbDbParseState *state,
                              PdbDocElementNode *element)
{
  const char *parent_name;
  char **att;

  parent_name = ((PdbDocElementNode *) element->node.parent)->name;

  /* Ignore the icon for the reference if the parent is one of the
   * following types. A comment in the code for the WebOS version of
   * PReVo says that the XSLT for ReVo does this too, but I can't seem
   * to find it anymore */
  if (!strcmp (parent_name, "dif") ||
      !strcmp (parent_name, "rim") ||
      !strcmp (parent_name, "ekz") ||
      !strcmp (parent_name, "klr"))
    return;

  for (att = element->atts; att[0]; att += 2)
    if (!strcmp (att[0], "tip"))
      {
        pdb_db_add_replacement (state,
                                att[1],
                                pdb_db_ref_types,
                                G_N_ELEMENTS (pdb_db_ref_types));

        break;
      }
}

static void
pdb_db_add_reference_span (PdbDb *db,
                           PdbDbParseState *state,
                           PdbSpanType span_type,
                           const char *mrk)
{
  pdb_db_start_text (state);

  PdbSpan *span = pdb_db_start_span (state, span_type);

  PdbDbLink *link = g_slice_new (PdbDbLink);
  link->span = span;
  link->string = state->string;

  PdbDbReference *reference = g_slice_new (PdbDbReference);
  reference->type = PDB_DB_REFERENCE_TYPE_MARK;
  reference->d.mark = g_strdup (mrk);
  link->reference = reference;

  db->links = g_list_prepend (db->links, link);
}

static gboolean
pdb_db_handle_ref (PdbDb *db,
                   PdbDbParseState *state,
                   PdbDocElementNode *element,
                   PdbSpan *span,
                   GError **error)
{
  char **att;

  for (att = element->atts; att[0]; att += 2)
    if (!strcmp (att[0], "cel"))
      goto found_cel;

  g_set_error (error,
               PDB_ERROR,
               PDB_ERROR_BAD_FORMAT,
               _("<ref> tag found with a cel attribute"));
  return FALSE;

 found_cel:

  pdb_db_handle_reference_type (state, element);

  pdb_db_add_reference_span (db, state, PDB_SPAN_REFERENCE, att[1]);

  return TRUE;
}

static gboolean
pdb_db_handle_refgrp (PdbDb *db,
                      PdbDbParseState *state,
                      PdbDocElementNode *element,
                      PdbSpan *span,
                      GError **error)
{
  pdb_db_handle_reference_type (state, element);

  return TRUE;
}

static const char *
pdb_db_get_sncref_ref (const PdbDocElementNode *element)
{
  const char *ref = pdb_doc_get_attribute (element, "ref");

  if (ref != NULL)
    return ref;

  /* Instead of specifying the target as an attribute, the sncref tag
   * can also be a child of a <ref> tag. */

  if (element->node.parent == NULL)
    return NULL;

  PdbDocElementNode *parent = (PdbDocElementNode *) element->node.parent;

  if (strcmp (parent->name, "ref"))
    return NULL;

  return pdb_doc_get_attribute (parent, "cel");
}

static gboolean
pdb_db_handle_sncref (PdbDb *db,
                      PdbDbParseState *state,
                      PdbDocElementNode *element,
                      GError **error)
{
  const char *ref = pdb_db_get_sncref_ref (element);

  if (ref == NULL)
    {
      g_set_error (error,
                   PDB_ERROR,
                   PDB_ERROR_BAD_FORMAT,
                   _("<sncref> tag found with a target"));
      return FALSE;
    }

  pdb_db_start_text (state);
  pdb_db_add_reference_span (db, state, PDB_SPAN_SUPERSCRIPT, ref);

  return TRUE;
}

static gboolean
pdb_db_handle_subdrv (PdbDb *db,
                      PdbDbParseState *state,
                      PdbDocElementNode *element,
                      PdbSpan *span,
                      GError **error)
{
  int sence_num;

  sence_num = pdb_db_get_element_num (element);

  /* We don't need to do anything if this is the only subdrv */
  if (sence_num != -1)
    {
      pdb_db_start_text (state);
      g_string_append_printf (state->buf, "%c. ", sence_num + 'A');
    }

  return TRUE;
}

static gboolean
pdb_db_handle_snc (PdbDb *db,
                   PdbDbParseState *state,
                   PdbDocElementNode *element,
                   PdbSpan *span,
                   GError **error)
{
  int sence_num;

  sence_num = pdb_db_get_element_num (element);

  /* We don't need to do anything if this is the only sence */
  if (sence_num != -1)
    {
      pdb_db_start_text (state);
      g_string_append_printf (state->buf, "%i. ", sence_num + 1);
    }

  return TRUE;
}

static gboolean
pdb_db_handle_subsnc (PdbDb *db,
                      PdbDbParseState *state,
                      PdbDocElementNode *element,
                      PdbSpan *span,
                      GError **error)
{
  int sence_num;

  sence_num = pdb_db_get_element_num (element);

  /* We don't need to do anything if this is the only subsence */
  if (sence_num != -1)
    {
      pdb_db_start_text (state);
      g_string_append_printf (state->buf, "%c) ", sence_num + 'a');
    }

  return TRUE;
}

static gboolean
pdb_db_handle_uzo (PdbDb *db,
                   PdbDbParseState *state,
                   PdbDocElementNode *element,
                   PdbSpan *span,
                   GError **error)
{
  char **att;

  for (att = element->atts; att[0]; att += 2)
    {
      if (!strcmp (att[0], "tip"))
        {
          if (!strcmp (att[1], "stl"))
            state->skip_children =
              pdb_db_add_replacement_contents (state,
                                               element,
                                               pdb_db_styles,
                                               G_N_ELEMENTS (pdb_db_styles));

          break;
        }
    }

  return TRUE;
}

static PdbDbElementSpan
pdb_db_element_spans[] =
  {
    { .name = "ofc", .type = PDB_SPAN_SUPERSCRIPT },
    { .name = "ekz", .type = PDB_SPAN_ITALIC },
    {
      .name = "subdrv",
      .type = PDB_SPAN_NONE,
      .handler = pdb_db_handle_subdrv,
      .paragraph = TRUE
    },
    {
      .name = "snc",
      .type = PDB_SPAN_NONE,
      .handler = pdb_db_handle_snc,
      .paragraph = TRUE
    },
    {
      .name = "subsnc",
      .type = PDB_SPAN_NONE,
      .handler = pdb_db_handle_subsnc,
      .paragraph = TRUE
    },
    {
      .name = "ref",
      .type = PDB_SPAN_NONE,
      .handler = pdb_db_handle_ref
    },
    {
      .name = "refgrp",
      .type = PDB_SPAN_NONE,
      .handler = pdb_db_handle_refgrp
    },
    {
      .name = "rim",
      .type = PDB_SPAN_NOTE,
      .handler = pdb_db_handle_rim,
      .paragraph = TRUE
    },
    { .name = "em", .type = PDB_SPAN_BOLD, },
    { .name = "aut", .type = PDB_SPAN_NONE, .handler = pdb_db_handle_aut },
    { .name = "uzo", .type = PDB_SPAN_NONE, .handler = pdb_db_handle_uzo },
    { .name = "trd", .type = PDB_SPAN_ITALIC, },
  };

static gboolean
pdb_db_should_ignore_spannable_tag (PdbDocElementNode *element)
{
  /* Skip citations, adm tags and pictures */
  if (!strcmp (element->name, "fnt") ||
      !strcmp (element->name, "adm") ||
      !strcmp (element->name, "bld") ||
      /* Ignore translation groups. They are handled separately */
      !strcmp (element->name, "trdgrp"))
    return TRUE;

  /* We want to ignore kap tags within a drv because they are handled
   * separately and parsed into the section header. However, a kap tag
   * can be found within an enclosing kap tag if it is a variation. In
   * that case we do want it to be processed because the section
   * header will be a comma-separated list. To detect this situation
   * we'll just check if any parents are a kap tag */
  if (!strcmp (element->name, "kap"))
    {
      PdbDocElementNode *parent;

      for (parent = (PdbDocElementNode *) element->node.parent;
           parent;
           parent = (PdbDocElementNode *) parent->node.parent)
        if (!strcmp (parent->name, "kap"))
          goto embedded_kap;

      /* If we make it here, the kap is a toplevel kap */
      return TRUE;
    }
 embedded_kap:

  /* trd tags are handled separately unless they are a direct child of
   * a definition. For example, in the article for ĉerizo the latin
   * name for the fruit is mentioned directly in the article as a
   * translation. In that case we do want to show the contents of the
   * tag. This is also how the XSL templates for ReVo filter the
   * tag. */
  if (!strcmp (element->name, "trd"))
    {
      PdbDocElementNode *parent;

      parent = (PdbDocElementNode *) element->node.parent;

      return parent == NULL || strcmp (parent->name, "dif");
    }

  return FALSE;
}

static gboolean
pdb_db_parse_node (PdbDb *db,
                   PdbDbParseState *state,
                   PdbDocNode *node,
                   GError **error)
{
  if (node->next)
    pdb_db_parse_push_node (state, node->next);

  switch (node->type)
    {
    case PDB_DOC_NODE_TYPE_ELEMENT:
      {
        PdbDocElementNode *element = (PdbDocElementNode *) node;

        if (!strcmp (element->name, "tld"))
          {
            pdb_db_start_text (state);
            pdb_db_append_tld (db, state->buf, element->atts);
          }
        else if (pdb_db_should_ignore_spannable_tag (element))
          {
            /* skip */
          }
        else if (element->node.first_child)
          {
            int i;

            state->skip_children = FALSE;

            for (i = 0; i < G_N_ELEMENTS (pdb_db_element_spans); i++)
              {
                const PdbDbElementSpan *elem_span =
                  pdb_db_element_spans + i;

                if (!strcmp (elem_span->name, element->name))
                  {
                    PdbSpan *span;

                    if (elem_span->paragraph)
                      {
                        /* Make sure there is a paragraph separator
                         * before and after the remark */
                        state->queued_space =
                          PDB_DB_QUEUED_SPACE_TYPE_PARAGRAPH;
                        pdb_db_parse_push_add_paragraph (state);
                      }

                    if (elem_span->type == PDB_SPAN_NONE)
                      span = NULL;
                    else
                      {
                        pdb_db_start_text (state);
                        span = pdb_db_start_span (state, elem_span->type);
                      }

                    if (elem_span->handler &&
                        !elem_span->handler (db,
                                             state,
                                             element,
                                             span,
                                             error))
                      return FALSE;

                    break;
                  }
              }

            if (!state->skip_children)
              pdb_db_parse_push_node (state,
                                      element->node.first_child);
          }
        else if (!strcmp (element->name, "sncref"))
          {
            if (!pdb_db_handle_sncref (db,
                                       state,
                                       element,
                                       error))
              return FALSE;
          }
      }
      break;

    case PDB_DOC_NODE_TYPE_TEXT:
      {
        PdbDocTextNode *text = (PdbDocTextNode *) node;
        const char *p, *end;

        for (p = text->data, end = text->data + text->len;
             p < end;
             p++)
          {
            if (g_ascii_isspace (*p))
              {
                if (state->queued_space == PDB_DB_QUEUED_SPACE_TYPE_NONE)
                  state->queued_space = PDB_DB_QUEUED_SPACE_TYPE_SPACE;
              }
            else
              {
                pdb_db_start_text (state);
                g_string_append_c (state->buf, *p);
              }
          }
      }
      break;
    }

  return TRUE;
}

static gboolean
pdb_db_parse_spannable_string_upto (PdbDb *db,
                                    PdbDocElementNode *root_element,
                                    PdbDocNode *upto,
                                    PdbDbSpannableString *string,
                                    GError **error)
{
  PdbDbParseState state;

  state.stack = g_array_new (FALSE, FALSE, sizeof (PdbDbParseStackEntry));
  state.buf = g_string_new (NULL);
  state.queued_space = PDB_DB_QUEUED_SPACE_TYPE_NONE;
  state.string = string;

  pdb_list_init (&state.spans);

  pdb_db_parse_push_node (&state, root_element->node.first_child);

  while (state.stack->len > 0)
    {
      PdbDbParseStackEntry this_entry =
        g_array_index (state.stack, PdbDbParseStackEntry, state.stack->len - 1);

      g_array_set_size (state.stack, state.stack->len - 1);

      switch (this_entry.type)
        {
        case PDB_DB_STACK_CLOSE_SPAN:
          this_entry.d.span->span_length =
            state.buf->len - this_entry.d.span->span_start;
          break;

        case PDB_DB_STACK_ADD_PARAGRAPH:
          state.queued_space = PDB_DB_QUEUED_SPACE_TYPE_PARAGRAPH;
          break;

        case PDB_DB_STACK_CLOSING_CHARACTER:
          pdb_db_start_text (&state);
          g_string_append_unichar (state.buf, this_entry.d.character);
          break;

        case PDB_DB_STACK_NODE:
          if (this_entry.d.node == upto)
            goto done;
          if (!pdb_db_parse_node (db, &state, this_entry.d.node, error))
            goto error;
          break;
        }
    }

 done:
  string->length = state.buf->len;
  string->text = g_string_free (state.buf, FALSE);
  pdb_list_init (&string->spans);
  pdb_list_insert_list (&string->spans, &state.spans);

  g_array_free (state.stack, TRUE);

  return TRUE;

 error:
  g_array_free (state.stack, TRUE);
  g_string_free (state.buf, TRUE);
  pdb_span_free_list (&state.spans);

  return FALSE;
}

static gboolean
pdb_db_parse_spannable_string (PdbDb *db,
                               PdbDocElementNode *root_element,
                               PdbDbSpannableString *string,
                               GError **error)
{
  return pdb_db_parse_spannable_string_upto (db,
                                             root_element,
                                             NULL, /* upto */
                                             string,
                                             error);
}

static void
pdb_db_add_kap_index (PdbDb *db,
                      PdbDocElementNode *kap,
                      PdbDbArticle *article,
                      PdbDbSection *section)
{
  GString *buf = g_string_new (NULL);
  const char *display_name, *real_name;
  PdbDocNode *node;
  PdbDbReference entry;

  for (node = kap->node.first_child; node; node = node->next)
    switch (node->type)
      {
      case PDB_DOC_NODE_TYPE_TEXT:
        {
          PdbDocTextNode *text_node = (PdbDocTextNode *) node;
          g_string_append_len (buf, text_node->data, text_node->len);
        }
        break;

      case PDB_DOC_NODE_TYPE_ELEMENT:
        {
          PdbDocElementNode *element = (PdbDocElementNode *) node;

          if (!strcmp (element->name, "tld") ||
              !strcmp (element->name, "rad"))
            pdb_db_append_tld (db, buf, element->atts);
          else if (!strcmp (element->name, "var"))
            {
              PdbDocElementNode *child_element;

              /* If the kap contains a variation with an embedded kap
               * then we'll recursively add that to the index too */
              child_element =
                pdb_doc_get_child_element (&element->node, "kap");

              if (child_element)
                pdb_db_add_kap_index (db, child_element, article, section);
            }
        }
        break;
      }

  entry.type = PDB_DB_REFERENCE_TYPE_DIRECT;
  entry.d.direct.article = article;
  entry.d.direct.section = section;

  pdb_trim_buf (buf);

  /* If the <kap> tag contains variations then it will be a list with
   * commas in. The variations are ignored in the index text so it
   * will end up as an empty blob of trailing commas. Lets trim
   * these */
  while (buf->len > 0 &&
         (buf->str[buf->len - 1] == ' ' ||
          buf->str[buf->len - 1] == ','))
    g_string_set_size (buf, buf->len - 1);

  if (buf->str[0] == '-' &&
      buf->str[1])
    {
      real_name = buf->str + 1;
      display_name = buf->str;
    }
  else
    {
      real_name = buf->str;
      display_name = NULL;
    }

  pdb_db_add_index_entry (db,
                          "eo", /* language code */
                          real_name,
                          display_name,
                          &entry);

  g_string_free (buf, TRUE);
}

static PdbDbSection *
pdb_db_parse_drv (PdbDb *db,
                  PdbDbArticle *article,
                  PdbDocElementNode *root_node,
                  GError **error)
{
  PdbDocElementNode *kap;
  PdbDbSection *section;
  gboolean result = TRUE;

  kap = pdb_doc_get_child_element (&root_node->node, "kap");

  if (kap == NULL)
    {
      g_set_error (error,
                   PDB_ERROR,
                   PDB_ERROR_BAD_FORMAT,
                   _("<drv> tag found with no <kap>"));
      return NULL;
    }

  section = pdb_db_section_new ("eo");

  pdb_db_add_kap_index (db, kap, article, section);

  if (pdb_db_parse_spannable_string (db, kap, &section->title, error))
    {
      if (!pdb_db_parse_spannable_string (db,
                                          root_node,
                                          &section->text,
                                          error))
        result = FALSE;

      if (!result)
        pdb_db_destroy_spannable_string (&section->title);
    }
  else
    result = FALSE;

  if (!result)
    {
      g_slice_free (PdbDbSection, section);
      return NULL;
    }
  else
    return section;
}

static gboolean
pdb_db_add_drv (PdbDb *db,
                PdbDbArticle *article,
                PdbDocElementNode *element,
                GQueue *sections,
                GError **error)
{
  PdbDbSection *section;
  PdbDbReference ref;

  section = pdb_db_parse_drv (db, article, element, error);

  if (section == NULL)
    return FALSE;

  g_queue_push_tail (sections, section);

  ref.type = PDB_DB_REFERENCE_TYPE_DIRECT;
  ref.d.direct.article = article;
  ref.d.direct.section = section;

  if (!pdb_db_find_translations_recursive (db,
                                           element,
                                           &ref,
                                           error))
    return FALSE;

  return TRUE;
}

static gboolean
pdb_db_parse_subart (PdbDb *db,
                     PdbDbArticle *article,
                     PdbDocElementNode *root_node,
                     GQueue *sections,
                     GError **error)
{
  PdbDocNode *node;
  PdbDocElementNode *drv;
  PdbDbSection *section;
  PdbDbReference ref;
  GString *buf;
  int subart_num;

  subart_num = pdb_db_get_element_num (root_node);

  if (subart_num == -1)
    /* It probably doesn't make any sense to have a <subart> on its
     * own, but the article for '-il' seems to do it anyway */
    subart_num = 0;

  section = pdb_db_section_new ("eo");

  buf = g_string_new (NULL);
  pdb_roman_to_text_append (subart_num + 1, buf);
  g_string_append (buf, ".");
  section->title.length = buf->len;
  section->title.text = g_string_free (buf, FALSE);
  pdb_list_init (&section->title.spans);

  ref.type = PDB_DB_REFERENCE_TYPE_DIRECT;
  ref.d.direct.article = article;
  ref.d.direct.section = section;

  /* The subart is assumed to be a spannable string followed by a list
   * of zero or more <drv>s */
  drv = pdb_doc_get_child_element (&root_node->node, "drv");

  if (!pdb_db_parse_spannable_string_upto (db,
                                           root_node,
                                           &drv->node, /* upto */
                                           &section->text,
                                           error))
    {
      pdb_db_destroy_spannable_string (&section->title);
      g_slice_free (PdbDbSection, section);
      return FALSE;
    }

  g_queue_push_tail (sections, section);

  if (!pdb_db_find_translations_recursive_skip (db,
                                                root_node,
                                                "drv",
                                                &ref,
                                                error))
    return FALSE;

  for (node = &drv->node; node; node = node->next)
    switch (node->type)
      {
      case PDB_DOC_NODE_TYPE_ELEMENT:
        {
          PdbDocElementNode *element = (PdbDocElementNode *) node;

          if (!strcmp (element->name, "drv"))
            {
              if (!pdb_db_add_drv (db,
                                   article,
                                   element,
                                   sections,
                                   error))
                return FALSE;
            }
          else if (strcmp (element->name, "adm") &&
                   strcmp (element->name, "trd") &&
                   strcmp (element->name, "trdgrp") &&
                   /* FIXME - this probably shouldn't strip out
                    * <rim> tags here */
                   strcmp (element->name, "rim"))
            {
              g_set_error (error,
                           PDB_ERROR,
                           PDB_ERROR_BAD_FORMAT,
                           _("<%s> tag found in <subart> that has a <drv>"),
                           element->name);
              return FALSE;
            }
        }
        break;

      case PDB_DOC_NODE_TYPE_TEXT:
        {
          PdbDocTextNode *text = (PdbDocTextNode *) node;
          int i;

          for (i = 0; i < text->len; i++)
            if (!g_ascii_isspace (text->data[i]))
              {
                g_set_error (error,
                             PDB_ERROR,
                             PDB_ERROR_BAD_FORMAT,
                             _("Unexpected bare text in <subart> that "
                               "has a <drv>"));
                return FALSE;
              }
        }
        break;
      }

  return TRUE;
}

static gboolean
pdb_db_parse_toplevel_dif (PdbDb *db,
                           PdbDbArticle *article,
                           PdbDocElementNode *root_node,
                           GQueue *sections,
                           GError **error)
{
  PdbDbSection *section;

  section = pdb_db_section_new ("eo");

  section->title.text = g_strdup ("Resumo");
  section->title.length = strlen (section->title.text);
  pdb_list_init (&section->title.spans);

  /* The toplevel definition should directly be a spannable string */
  if (!pdb_db_parse_spannable_string (db,
                                      (PdbDocElementNode *) root_node,
                                      &section->text,
                                      error))
    {
      pdb_db_destroy_spannable_string (&section->title);
      g_slice_free (PdbDbSection, section);
      return FALSE;
    }

  g_queue_push_tail (sections, section);

  return TRUE;
}

static gboolean
get_var_root (PdbDb *db,
              PdbDocElementNode *var,
              GError **error)
{
  PdbDocElementNode *kap = pdb_doc_get_child_element (&var->node, "kap");
  PdbDocElementNode *rad;
  const char *label;

  if (kap == NULL)
    {
      g_set_error (error,
                   PDB_ERROR,
                   PDB_ERROR_BAD_FORMAT,
                   _("<var> found in <kap> with no inner <kap>"));
      return FALSE;
    }

  rad = pdb_doc_get_child_element (&kap->node, "rad");

  /* Sometimes the variant isn’t marked with a <rad> element and the
   * value is just spelled out explicitly in the article. */
  if (rad == NULL)
    return TRUE;

  label = pdb_doc_get_attribute (rad, "var");

  /* Some articles have a variant that isn’t used in the body of the
   * article */
  if (label == NULL)
    return TRUE;

  g_hash_table_insert (db->root_variants,
                       g_strdup (label),
                       pdb_doc_get_element_text (rad));

  return TRUE;
}

static gboolean
get_roots (PdbDb *db,
           PdbDocElementNode *kap,
           GError **error)
{
  PdbDocNode *n;
  PdbDocElementNode *elem;
  char *word_root = NULL;

  for (n = kap->node.first_child; n; n = n->next)
    {
      if (n->type != PDB_DOC_NODE_TYPE_ELEMENT)
        continue;

      elem = (PdbDocElementNode *) n;

      if (!strcmp (elem->name, "rad"))
        {
          if (word_root != NULL)
            {
              g_set_error (error,
                           PDB_ERROR,
                           PDB_ERROR_BAD_FORMAT,
                           _("Multiple <rad> elements found in <kap> with no "
                             "var attribute"));
              goto error;
            }

          word_root = pdb_doc_get_element_text (elem);
        }
      else if (!strcmp (elem->name, "var"))
        {
          if (!get_var_root (db, elem, error))
            goto error;
        }
    }

  if (word_root == NULL)
    {
      g_set_error (error,
                   PDB_ERROR,
                   PDB_ERROR_BAD_FORMAT,
                   _("<kap> tag found with no <rad>"));
      goto error;
    }

  db->word_root = word_root;

  return TRUE;

 error:
  g_free (word_root);
  g_hash_table_remove_all (db->root_variants);
  return FALSE;
}

static PdbDbArticle *
pdb_db_parse_article (PdbDb *db,
                      PdbDocElementNode *root_node,
                      GError **error)
{
  PdbDbArticle *article;
  gboolean result = TRUE;
  PdbDocElementNode *kap;
  GQueue sections;

  kap = pdb_doc_get_child_element (&root_node->node, "kap");

  if (kap == NULL)
    {
      g_set_error (error,
                   PDB_ERROR,
                   PDB_ERROR_BAD_FORMAT,
                   _("<art> tag found with no <kap>"));
      return NULL;
    }

  if (!get_roots (db, kap, error))
    return NULL;

  g_queue_init (&sections);

  article = g_slice_new (PdbDbArticle);

  if (pdb_db_parse_spannable_string (db, kap, &article->title, error))
    {
      PdbDocNode *node;

      for (node = root_node->node.first_child; node; node = node->next)
        if (node->type == PDB_DOC_NODE_TYPE_ELEMENT)
          {
            PdbDocElementNode *element = (PdbDocElementNode *) node;

            if (!strcmp (element->name, "drv"))
              {
                if (!pdb_db_add_drv (db,
                                     article,
                                     element,
                                     &sections,
                                     error))
                  {
                    result = FALSE;
                    break;
                  }
              }
            else if (!strcmp (element->name, "subart"))
              {
                if (!pdb_db_parse_subart (db,
                                          article,
                                          element,
                                          &sections,
                                          error))
                  {
                    result = FALSE;
                    break;
                  }
              }
            else if (!strcmp (element->name, "dif"))
              {
                if (!pdb_db_parse_toplevel_dif (db,
                                                article,
                                                element,
                                                &sections,
                                                error))
                  {
                    result = FALSE;
                    break;
                  }
              }
          }

      if (result)
        {
          /* If we didn't find any content then we'll just treat
           * the whole article as if it were a drv */
          if (sections.head == NULL)
            {
              if (!pdb_db_add_drv (db, article, root_node, &sections, error))
                result = FALSE;
            }
          else
            {
              PdbDbReference ref;

              ref.type = PDB_DB_REFERENCE_TYPE_DIRECT;
              ref.d.direct.article = article;
              ref.d.direct.section = sections.head->data;

              if (!pdb_db_find_translations (db,
                                             root_node,
                                             &ref,
                                             error))
                result = FALSE;
            }

          if (result)
            {
              GList *translations = pdb_db_flush_translations (db);
              int length = g_list_length (translations);

              if (length > 0)
                {
                  if (sections.head == NULL)
                    sections.head = translations;
                  else
                    {
                      sections.tail->next = translations;
                      translations->prev = sections.tail;
                    }

                  sections.tail = g_list_last (translations);
                  sections.length += length;
                }
            }
        }

      if (!result)
        pdb_db_destroy_spannable_string (&article->title);
    }
  else
    result = FALSE;

  g_free (db->word_root);
  g_hash_table_remove_all (db->root_variants);

  if (result)
    {
      article->sections = sections.head;
      return article;
    }
  else
    {
      pdb_db_free_section_list (sections.head);
      g_slice_free (PdbDbArticle, article);
      return NULL;
    }
}

static gboolean
pdb_db_parse_articles (PdbDb *db,
                       PdbDocElementNode *root_node,
                       GError **error)
{
  PdbDocNode *node;

  for (node = root_node->node.first_child; node; node = node->next)
    if (node->type == PDB_DOC_NODE_TYPE_ELEMENT)
      {
        PdbDocElementNode *element = (PdbDocElementNode *) node;

        if (!strcmp (element->name, "art"))
          {
            PdbDbArticle *article = pdb_db_parse_article (db, element, error);

            if (article == NULL)
              return FALSE;

            g_ptr_array_add (db->articles, article);
          }
      }

  return TRUE;
}

static void
pdb_db_free_data_cb (void *data,
                     void *user_data)
{
  pdb_db_reference_free (data);
}

static void
pdb_db_get_reference_cb (void *data,
                         int *article_num,
                         int *mark_num,
                         void *user_data)
{
  PdbDb *db = user_data;
  PdbDbReference *ref = data;
  int sence_num;

  pdb_db_resolve_reference (db,
                            ref,
                            article_num,
                            mark_num,
                            &sence_num);
}

static void
pdb_db_add_file_root_mark (PdbDb *db,
                           const char *filename,
                           PdbDbArticle *article)
{
  char *mark_name;

  if (article->sections == NULL)
    return;

  mark_name = g_path_get_basename (filename);

  /* Strip off the extension */
  if (g_str_has_suffix (mark_name, ".xml"))
    mark_name[strlen (mark_name) - 4] = '\0';

  pdb_db_add_mark (db,
                   article,
                   article->sections->data,
                   mark_name);

  g_free (mark_name);
}

PdbDb *
pdb_db_new (PdbRevo *revo,
            GError **error)
{
  PdbDb *db;
  char **files;
  GError *tmp_error;

  db = g_slice_new (PdbDb);

  db->lang = pdb_lang_new (revo,
                           pdb_db_free_data_cb,
                           pdb_db_get_reference_cb,
                           db,
                           error);

  if (db->lang == NULL)
    {
      g_slice_free (PdbDb, db);
      return NULL;
    }

  db->articles = g_ptr_array_new ();
  db->links = NULL;

  db->marks = g_hash_table_new_full (g_str_hash,
                                     g_str_equal,
                                     g_free,
                                     (GDestroyNotify) pdb_db_mark_free);

  db->translations = g_hash_table_new_full (g_str_hash,
                                            g_str_equal,
                                            (GDestroyNotify) g_free,
                                            pdb_db_free_translation_data_cb);

  db->root_variants = g_hash_table_new_full (g_str_hash,
                                             g_str_equal,
                                             g_free,
                                             g_free);

  files = pdb_revo_list_files (revo, "revo/*.xml", error);

  if (files == NULL)
    {
      pdb_db_free (db);
      db = NULL;
    }
  else
    {
      char **file_p;

      for (file_p = files; *file_p; file_p++)
        {
          const char *file = *file_p;
          PdbDoc *doc;

          if ((doc = pdb_doc_load (revo, file, error)) == NULL)
            {
              pdb_db_free (db);
              db = NULL;
              break;
            }
          else
            {
              gboolean parse_result;
              int old_len = db->articles->len;

              parse_result = pdb_db_parse_articles (db,
                                                    pdb_doc_get_root (doc),
                                                    error);
              pdb_doc_free (doc);

              if (parse_result)
                {
                  if (db->articles->len <= old_len)
                    fprintf (stderr,
                             _("no articles found in %s\n"),
                             file);
                  else
                    {
                      /* Some articles directly reference the filename
                       * instead of a real mark so we need to add a mark
                       * for each file */
                      PdbDbArticle *article =
                        g_ptr_array_index (db->articles, db->articles->len - 1);

                      pdb_db_add_file_root_mark (db,
                                                 file,
                                                 article);
                    }
                }
              else
                {
                  pdb_db_free (db);
                  db = NULL;

                  /* Add the filename into the error message */
                  if (error && *error && (*error)->domain == PDB_ERROR)
                    {
                      tmp_error = g_error_new ((*error)->domain,
                                               (*error)->code,
                                               "%s: %s",
                                               file,
                                               (*error)->message);
                      g_error_free (*error);
                      *error = tmp_error;
                    }
                  break;
                }
            }
        }

      g_strfreev (files);
    }

  if (db)
    pdb_db_resolve_links (db);

  return db;
}

static void
pdb_db_free_link_cb (void *ptr,
                     void *user_data)
{
  pdb_db_link_free (ptr);
}

static void
pdb_db_free_link_list (GList *links)
{
  g_list_foreach (links, pdb_db_free_link_cb, NULL);
  g_list_free (links);
}

void
pdb_db_free (PdbDb *db)
{
  int i;

  pdb_lang_free (db->lang);

  for (i = 0; i < db->articles->len; i++)
    {
      PdbDbArticle *article = g_ptr_array_index (db->articles, i);

      pdb_db_destroy_spannable_string (&article->title);
      pdb_db_free_section_list (article->sections);

      g_slice_free (PdbDbArticle, article);
    }

  g_ptr_array_free (db->articles, TRUE);

  pdb_db_free_link_list (db->links);

  g_hash_table_destroy (db->marks);
  g_hash_table_destroy (db->translations);
  g_hash_table_destroy (db->root_variants);

  g_slice_free (PdbDb, db);
}

static int
pdb_db_get_utf16_length (const char *buf,
                         int byte_length)
{
  const char *p;
  int length = 0;

  /* Calculates the length that the string would have if it was
   * encoded in UTF-16 */
  for (p = buf; p - buf < byte_length; p = g_utf8_next_char (p))
    {
      gunichar ch = g_utf8_get_char (p);

      length++;
      /* If the character is outside the BMP then it
       * will need an extra 16 bit number to encode
       * it */
      if (ch >= 0x10000)
        length++;
    }

  return length;
}

static gboolean
pdb_db_write_string (PdbDb *pdb,
                     const PdbDbSpannableString *string,
                     gboolean single,
                     PdbFile *out,
                     GError **error)
{
  PdbSpan *span;

  if (!pdb_file_write_16 (out, string->length, error) ||
      !pdb_file_write (out, string->text, string->length, error))
    return FALSE;

  pdb_list_for_each (span, &string->spans, link)
    {
      /* Ignore empty spans */
      if (span->span_length > 0)
        {
          guint16 span_length =
            single ?
            span->span_length :
            pdb_db_get_utf16_length (string->text + span->span_start,
                                     span->span_length);
          guint16 span_start =
            single ?
            span->span_start :
            pdb_db_get_utf16_length (string->text, span->span_start);
          guint16 v[4] = { GUINT16_TO_LE (span_length),
                           GUINT16_TO_LE (span_start),
                           GUINT16_TO_LE (span->data1),
                           GUINT16_TO_LE (span->data2) };

          if (!pdb_file_write (out, v, sizeof (v), error) ||
              !pdb_file_write_8 (out, span->type, error))
            return FALSE;
        }
    }

  return pdb_file_write_16 (out, 0, error);
}

static gboolean
pdb_db_save_article (PdbDb *db,
                     PdbDbArticle *article,
                     gboolean single,
                     PdbFile *out,
                     GError **error)
{
  GList *sl;
  int old_pos = out->pos;
  int article_len;

  /* Leave space for the article size */
  if (!pdb_file_seek (out,
                      sizeof (guint32),
                      SEEK_CUR,
                      error))
    return FALSE;

  if (!pdb_db_write_string (db, &article->title, single, out, error))
    return FALSE;

  for (sl = article->sections; sl; sl = sl->next)
    {
      PdbDbSection *section = sl->data;

      if (!pdb_file_write (out, section->lang_code, 3, error) ||
          !pdb_db_write_string (db, &section->title, single, out, error) ||
          !pdb_db_write_string (db, &section->text, single, out, error))
        return FALSE;
    }

  /* Fill in the article size */
  article_len = out->pos - old_pos - sizeof (guint32);
  if (!pdb_file_seek (out,
                      old_pos,
                      SEEK_SET,
                      error) ||
      !pdb_file_write_32 (out, article_len, error) ||
      !pdb_file_seek (out, article_len, SEEK_CUR, error))
    return FALSE;

  return TRUE;
}

gboolean
pdb_db_save (PdbDb *db,
             const char *dir,
             GError **error)
{
  gboolean ret = TRUE;
  int group_num;

  if (!pdb_lang_save (db->lang, dir, error))
    return FALSE;

  if (pdb_try_mkdir (error, dir, "assets", "articles", NULL))
    {
      int max_group = (db->articles->len + 15) / 16;

      for (group_num = 0; group_num < max_group; group_num++)
        {
          PdbFile out;
          char *article_name =
            g_strdup_printf ("article-%03xx.bin", group_num);
          gboolean write_status = TRUE;
          char *full_name = g_build_filename (dir,
                                              "assets",
                                              "articles",
                                              article_name,
                                              NULL);

          if (!pdb_file_open (&out, full_name, PDB_FILE_MODE_WRITE, error))
            write_status = FALSE;
          else
            {
              int page_num, max_page;

              if ((db->articles->len & 0xf) && group_num + 1 >= max_group)
                max_page = db->articles->len & 0xf;
              else
                max_page = 16;

              for (page_num = 0; page_num < max_page; page_num++)
                {
                  PdbDbArticle *article =
                    g_ptr_array_index (db->articles, group_num * 16 + page_num);

                  if (!pdb_db_save_article (db,
                                            article,
                                            FALSE, /* single */
                                            &out,
                                            error))
                    {
                      write_status = FALSE;
                      break;
                    }
                }

              if (!pdb_file_close (&out, write_status ? error : NULL))
                write_status = FALSE;
            }

          g_free (full_name);
          g_free (article_name);

          if (!write_status)
            {
              ret = FALSE;
              break;
            }
        }
    }
  else
    ret = FALSE;

  return ret;
}

gboolean
pdb_db_save_single (PdbDb *db,
                    const char *filename,
                    GError **error)
{
  PdbFile file;
  gboolean ret = TRUE;

  if (!pdb_file_open (&file, filename, PDB_FILE_MODE_WRITE, error))
    ret = FALSE;
  else
    {
      if (pdb_file_write (&file, pdb_db_magic, sizeof (pdb_db_magic), error) &&
          pdb_file_write_32 (&file, db->articles->len, error) &&
          /* Skip past the article offset table which we'll fill in
           * later */
          pdb_file_seek (&file, db->articles->len * 4, SEEK_CUR, error) &&
          pdb_lang_save_single (db->lang, &file, error))
        {
          int i;
          guint32 *offset_table = g_alloca (db->articles->len *
                                            sizeof (guint32));

          for (i = 0; i < db->articles->len; i++)
            {
              PdbDbArticle *article = g_ptr_array_index (db->articles, i);

              offset_table[i] = GUINT32_TO_LE (file.pos);

              if (!pdb_db_save_article (db,
                                        article,
                                        TRUE, /* single */
                                        &file,
                                        error))
                {
                  ret = FALSE;
                  goto done;
                }
            }

          if (!pdb_file_seek (&file,
                              sizeof (pdb_db_magic) + sizeof (guint32),
                              SEEK_SET,
                              error) ||
              !pdb_file_write (&file,
                               offset_table,
                               db->articles->len * sizeof (guint32),
                               error))
            ret = FALSE;

        done:
          (void) 0;
        }
      else
        ret = FALSE;

      if (!pdb_file_close (&file, ret ? error : NULL))
        ret = FALSE;

      if (!ret)
        g_unlink (filename);
    }

  return ret;
}
