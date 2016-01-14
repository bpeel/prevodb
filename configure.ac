AC_INIT([prevodb], 0.1)

AM_CONFIG_HEADER([config.h])

AM_INIT_AUTOMAKE([1.11 foreign])
AC_CONFIG_MACRO_DIR([autotools])

m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

AC_PROG_CC

# ****
# i18n
# ****

AS_ALL_LINGUAS
GETTEXT_PACKAGE=prevo
AC_SUBST(GETTEXT_PACKAGE)
AC_DEFINE_UNQUOTED(GETTEXT_PACKAGE, "$GETTEXT_PACKAGE",
                   [The gettext package name])
AM_GNU_GETTEXT_VERSION([0.17])
AM_GNU_GETTEXT([external])

CFLAGS="$CFLAGS -Wcast-align -Wuninitialized -Wno-strict-aliasing -Wempty-body -Wformat -Wformat-security -Winit-self -Wvla -Wpointer-arith -Wmissing-declarations -Wdeclaration-after-statement"

PKG_CHECK_MODULES(GLIB, [glib-2.0])
PKG_CHECK_MODULES(EXPAT, [expat])

AC_CONFIG_FILES([
        Makefile
        src/Makefile
        po/Makefile.in
])

AC_OUTPUT
