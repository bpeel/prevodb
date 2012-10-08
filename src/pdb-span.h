/*
 * PReVo - A portable version of ReVo for Android
 * Copyright (C) 2012  Neil Roberts
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

#ifndef PDB_SPAN_H
#define PDB_SPAN_H

#include <glib.h>

#include "pdb-list.h"

typedef enum
{
  PDB_SPAN_REFERENCE,
  PDB_SPAN_SUPERSCRIPT,
  PDB_SPAN_ITALIC,
  PDB_SPAN_NOTE,
  PDB_SPAN_BOLD,
  PDB_SPAN_NONE
} PdbSpanType;

typedef struct
{
  PdbList link;

  /* The span length and span start count the number of bytes. Once
   * stored to the file for an Android database, this will be
   * converted to a number of 16-bit units as if the string was stored
   * in UTF-16. */
  guint16 span_length;
  guint16 span_start;
  guint16 data1;
  guint16 data2;
  PdbSpanType type;
} PdbSpan;

void
pdb_span_free (PdbSpan *span);

void
pdb_span_free_list (PdbList *spans);

#endif /* PDB_SPAN_H */
