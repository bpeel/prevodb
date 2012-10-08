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

#include "pdb-span.h"

void
pdb_span_free (PdbSpan *span)
{
  g_slice_free (PdbSpan, span);
}

void
pdb_span_free_list (PdbList *spans)
{
  PdbSpan *span, *tmp;

  pdb_list_for_each_safe (span, tmp, spans, link)
    pdb_span_free (span);
}
