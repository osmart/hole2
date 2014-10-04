#!/bin/csh
echo
echo "# link hole ***"
echo "f77 check_number_of_atoms_in_pdb_file.f  hole.a -o ../exe/check_number_of_atoms_in_pdb_file"
f77 check_number_of_atoms_in_pdb_file.f  hole.a -o ../exe/check_number_of_atoms_in_pdb_file
