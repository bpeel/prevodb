/*
 * PReVo - A portable version of ReVo for Android
 * Copyright (C) 2012, 2013  Neil Roberts
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

#include "pdb-trim.h"

void
pdb_trim_buf (GString *buf)
{
  char *dst;
  const char *src;

  /* Skip leading spaces and replacing all sets of whitespace
   * characters with a single space */
  for (dst = buf->str, src = buf->str;
       *src;
       src++)
    if (g_ascii_isspace (*src))
      {
        if (dst > buf->str && dst[-1] != ' ')
          *(dst)++ = ' ';
      }
    else
      *(dst++) = *src;

  /* Remove any trailing space */
  if (dst > buf->str && dst[-1] == ' ')
    dst--;

  g_string_set_size (buf, dst - buf->str);
}
