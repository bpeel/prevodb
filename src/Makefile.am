bin_PROGRAMS = \
	prevodb \
	prevo

AM_CPPFLAGS = \
	-DPREVO_LOCALEDIR=\""$(localedir)"\" \
	$(GLIB_CFLAGS) \
	$(EXPAT_CFLAGS) \
	$(NULL)

AM_CFLAGS = \
	$(WARNING_CFLAGS) \
	$(NULL)

completiondir = $(sysconfdir)/bash_completion.d
dist_completion_DATA = \
	prevo-completion
	$(NULL)

prevodb_SOURCES = \
	main.c \
	pdb-db.c \
	pdb-db.h \
	pdb-doc.c \
	pdb-doc.h \
	pdb-error.c \
	pdb-error.h \
	pdb-file.c \
	pdb-file.h \
	pdb-lang.c \
	pdb-lang.h \
	pdb-list.c \
	pdb-list.h \
	pdb-mkdir.c \
	pdb-mkdir.h \
	pdb-revo.c \
	pdb-revo.h \
	pdb-roman.c \
	pdb-roman.h \
	pdb-span.c \
	pdb-span.h \
	pdb-strcmp.c \
	pdb-strcmp.h \
	pdb-trie.c \
	pdb-trie.h \
	pdb-trim.c \
	pdb-trim.h \
	pdb-xml.c \
	pdb-xml.h \
	$(NULL)

prevodb_LDADD = \
	$(GLIB_LIBS) \
	$(EXPAT_LIBS) \
	$(NULL)

prevo_SOURCES = \
	pdb-file.c \
	pdb-file.h \
	pdb-list.c \
	pdb-list.h \
	pdb-man.c \
	pdb-man.h \
	pdb-span.c \
	pdb-span.h \
	prevo.c

prevo_LDADD = \
	$(GLIB_LIBS) \
	$(NULL)

noinst_PROGRAMS = \
	dump-article \
	dump-index \
	dump-marks \
	test-doc \
	$(NULL)

dump_index_SOURCES = \
	dump-index.c

dump_index_LDADD = \
	$(GLIB_LIBS) \
	$(NULL)

dump_article_SOURCES = \
	dump-article.c \
	$(NULL)

dump_article_LDADD = \
	$(GLIB_LIBS) \
	$(NULL)

test_doc_SOURCES = \
	pdb-doc.c \
	pdb-error.c \
	pdb-revo.c \
	pdb-xml.c \
	test-doc.c \
	$(NULL)

test_doc_LDADD = \
	$(GLIB_LIBS) \
	$(EXPAT_LIBS) \
	$(NULL)

dump_marks_SOURCES = \
	dump-marks.c \
	pdb-error.c \
	pdb-revo.c \
	pdb-xml.c \
	$(NULL)

dump_marks_LDADD = \
	$(GLIB_LIBS) \
	$(EXPAT_LIBS) \
	$(NULL)
