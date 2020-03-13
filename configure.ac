AC_INIT([prevodb], 0.2)

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
AM_GNU_GETTEXT_VERSION([0.19])
AM_GNU_GETTEXT([external])

ALL_WARNING_CFLAGS="-Wall -Wuninitialized -Wempty-body -Wformat
                    -Wformat-security -Winit-self -Wundef
                    -Wvla -Wpointer-arith -Wmissing-declarations
                    -Wcast-align -Wno-strict-aliasing"
AS_COMPILER_FLAGS([WARNING_CFLAGS],
                  [$ALL_WARNING_CFLAGS])
AC_SUBST([WARNING_CFLAGS])

PKG_CHECK_MODULES(GLIB, [glib-2.0])
PKG_CHECK_MODULES(EXPAT, [expat])

AC_CONFIG_FILES([
        Makefile
        src/Makefile
        po/Makefile.in
])

AC_OUTPUT
