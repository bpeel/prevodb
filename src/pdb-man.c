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

#include "config.h"

#include <glib.h>
#include <signal.h>
#include <sys/wait.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>

#include "pdb-man.h"

static const char *
pdb_man_args[] =
  {
    "man", "-l", "-", NULL
  };

struct _PdbMan
{
  GPid pid;
  FILE *stdin_stream;
};

static void
kill_process (GPid pid)
{
  int status;

  if (waitpid (pid, &status, WNOHANG) <= 0)
    {
      kill (pid, SIGTERM);

      while (waitpid (pid, &status, 0) == -1 &&
             errno == EINTR);
    }
}

PdbMan *
pdb_man_new (GError **error)
{
  GPid pid;
  int stdin_fd;

  if (g_spawn_async_with_pipes (NULL, /* working_dir */
                                (char **) pdb_man_args,
                                NULL, /* envp */
                                G_SPAWN_DO_NOT_REAP_CHILD |
                                G_SPAWN_SEARCH_PATH,
                                NULL, /* child_setup_func */
                                NULL, /* user_data */
                                &pid,
                                &stdin_fd,
                                NULL, /* standard_output */
                                NULL, /* standard_error */
                                error))
    {
      FILE *stdin_stream;

      stdin_stream = fdopen (stdin_fd, "w");

      if (stdin_stream == NULL)
        {
          g_set_error (error,
                       G_FILE_ERROR,
                       g_file_error_from_errno (errno),
                       "Error starting groff: %s",
                       strerror (errno));
          close (stdin_fd);
          kill_process (pid);

          return NULL;
        }
      else
        {
          PdbMan *groff = g_slice_new (PdbMan);

          groff->stdin_stream = stdin_stream;
          groff->pid = pid;

          return groff;
        }
    }
  else
    return NULL;
}

gboolean
pdb_man_display (PdbMan *groff,
                 GError **error)
{
  int wait_ret;
  int status;
  int close_ret;

  close_ret = fclose (groff->stdin_stream);
  groff->stdin_stream = NULL;

  if (close_ret == EOF)
    {
      g_set_error (error,
                   G_FILE_ERROR,
                   g_file_error_from_errno (errno),
                   "Error writing to groff: %s", strerror (errno));
      return FALSE;
    }

  while ((wait_ret = waitpid (groff->pid, &status, 0)) == -1 &&
         errno == EINTR);

  groff->pid = 0;

  if (wait_ret == -1)
    {
      g_set_error (error,
                   G_FILE_ERROR,
                   g_file_error_from_errno (errno),
                   "Error waiting for groff: %s", strerror (errno));
      return FALSE;
    }

  if (status != 0)
    {
      g_set_error (error,
                   PDB_MAN_ERROR,
                   PDB_MAN_ERROR_STATUS,
                   "Failed to run groff");
      return FALSE;
    }

  return TRUE;
}

FILE *
pdb_man_get_output (PdbMan *groff)
{
  return groff->stdin_stream;
}

void
pdb_man_free (PdbMan *groff)
{
  if (groff->stdin_stream)
    fclose (groff->stdin_stream);

  if (groff->pid)
    kill_process (groff->pid);

  g_slice_free (PdbMan, groff);
}

GQuark
pdb_man_error_quark (void)
{
  return g_quark_from_static_string ("pdb-man-error-quark");
}
