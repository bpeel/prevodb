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

#ifndef PDB_MAN_H
#define PDB_MAN_H

#include <glib.h>
#include <stdio.h>

#define PDB_MAN_ERROR (pdb_man_error_quark ())

typedef struct _PdbMan PdbMan;

typedef enum
{
  PDB_MAN_ERROR_STATUS
} PbbGroffError;

PdbMan *
pdb_man_new (GError **error);

gboolean
pdb_man_display (PdbMan *groff,
                 GError **error);

FILE *
pdb_man_get_output (PdbMan *groff);

void
pdb_man_free (PdbMan *groff);

GQuark
pdb_man_error_quark (void);

#endif /* PDB_MAN_H */
