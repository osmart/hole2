/* program to produce vrml output from HOLE .sph
   format input file written by 
   Xiaonan Wang and Oliver Smart from July 1996 */
/* standard headers */
# include <stdio.h>
# include <stdlib.h>
# include <math.h>
# include <string.h>

/* file pointers */
FILE *input_sphfile;
FILE *output_wrl_file;

void main()
{
	int i;
	double X0=0, Y0=0,Z0=0;
	double X1,Y1,Z1,R1,Dx,Dy,Dz;
	char word[15][15];
	char filename[50]; /* input filename */
	char fileout[50]; /* output filename */
	char reply[20];
	double rcut1 = -1000., rcut2=-1000.;  /* default values - all to green */

	/* write greeting */
	printf("\n *** program sph_to_vrml ***");
	printf("\n Reads a .sph format input file produced by HOLE");
	printf("\n and produces a virtual reality markup language");
	printf("\n equivalent. NOTE CAPSULE OPTION NOT YET SUPPORTED");
	printf("\n\n Version H2alpha1");
	printf("\n(c) 1996 Oliver Smart & Birkbeck College, All rights reserved.\n\n");


	/* ask for input filename */
	printf ("\n enter input filename <abort>: ");
	gets (filename);
	while ((input_sphfile=fopen(filename,"r"))==NULL)
	{
		/* default is to abort */
		if (strlen(filename)==0) exit(0);
		printf ("\n Error cannot open file %c",7);
		printf ("\n Enter input filename <abort>: ");
		gets(filename);
	}

	/* ask for output filename */
	/* find last occurence of a "." in filename replacing it with .wrl*/
	strcpy(strrchr(filename,'.'), ".wrl");
	printf(" Enter output filename <%s>: ", filename);
	gets (fileout);
	/* default is for filename */
	if (strlen(fileout)==0) strcpy( fileout, filename);
	while ((output_wrl_file=fopen(fileout,"w"))==NULL)
	{
		printf ("\n Error cannot open file %c",7);
		printf("\n Enter output filename <%s>: ", filename);
		gets(fileout);
		/* default is for filename */
		if (strlen(fileout)==0) strcpy( fileout, filename);
	}

	/* ask whether the result should be coloured by radius */
	printf("\n Do you want to colour the file according to radius? (y/n) <no>: ");
	gets( reply);
	if ( (reply[0]=='y') || (reply[0]=='Y'))
	{/* YES wants to colour by radius */
		rcut1 = 1.15;
		rcut2=2.3;
		printf("\n Under what radius do want the surface to be rendered red? <%4.2f>: ", rcut1);
		gets( reply);
		if (strlen(reply)!=0) rcut1 = atof(reply);
		printf("\n Will colour surface green between this and <%4.2f>: ", rcut2);
		gets( reply);
		if (strlen(reply)!=0) rcut2 = atof(reply);
		printf("\n Will colour surface blue above this\n");
	}

	/* write header of to result file */
	fprintf( output_wrl_file,"#VRML V1.0 ascii\n");


	/* first pass for red spheres (under rcut1) */
	/* only do if doing different coloured spheres */
	if (rcut1 != -1000.)
	{
		X0 = 0.;
		Y0 = 0.;
		Z0 = 0.;
		fprintf( output_wrl_file,"Separator {\n");
		fprintf( output_wrl_file,"# red spheres\n");
		fprintf( output_wrl_file,"Material { diffuseColor 1 0 0 # Red\n");
		fprintf( output_wrl_file,"           ambientColor 1 0 0 \n");
		fprintf( output_wrl_file,"           specularColor 0.800 0.800 0.800 \n");
		fprintf( output_wrl_file,"           shininess 0.750 \n");
		fprintf( output_wrl_file,"         }\n");

		while (fscanf(input_sphfile,"%s",word[0])!=EOF)
		{
			if (strcmp(word[0],"ATOM")==0)
			{
				for (i=1; i<11; i++)
				{
					fscanf(input_sphfile, "%s",word[i]);
				}
				if ( (strcmp(word[2],"QSS")!=0) || (strcmp(word[3],"SPH")!=0) )
				{
					/* ignore - warn on green */
				}
				else
				{
					X1=atof(word[6]);
					Y1=atof(word[7]);
					Z1=atof(word[8]);
					R1=atof(word[9]);
					Dx=X1-X0;
					Dy=Y1-Y0;
					Dz=Z1-Z0;
					/* radius must be below rcut1 for red */
					if (  R1 < rcut1  )
					{
						fprintf(output_wrl_file,"\n Translation { translation %8.3f%8.3f%8.3f }",
						    Dx,Dy,Dz);
						fprintf(output_wrl_file,"\n	        Sphere { radius %8.3f }",R1);
						X0=X1; /*have output so store */
						Y0=Y1;
						Z0=Z1;
					}
				}
			}


		}
		/* end of reading input file */
		fprintf(output_wrl_file,"\n} # end of red spheres\n");
		/* rewind input file (i.e. go back to the first line */
		rewind( input_sphfile);

	}
	/* second pass for green spheres (under rcut1) */
	X0 = 0.;
	Y0 = 0.;
	Z0 = 0.;
	fprintf( output_wrl_file,"Separator {\n");
	fprintf( output_wrl_file,"# next green spheres\n");
	fprintf( output_wrl_file,"Material { diffuseColor 0 1 0 # green\n");
	fprintf( output_wrl_file,"           ambientColor 0 1 0 \n");
	fprintf( output_wrl_file,"           specularColor 0.800 0.800 0.800 \n");
	fprintf( output_wrl_file,"           shininess 0.750 \n");
	fprintf( output_wrl_file,"         }\n");

	while (fscanf(input_sphfile,"%s",word[0])!=EOF)
	{
		if (strcmp(word[0],"ATOM")==0)
		{
			for (i=1; i<11; i++)
			{
				fscanf(input_sphfile, "%s",word[i]);
			}
			if ( (strcmp(word[2],"QSS")!=0) || (strcmp(word[3],"SPH")!=0) )
			{
				printf("\n %cignoring atom %s %s as not valid in a .sph file", 7, word[2], word[3]);
			}
			else
			{
				X1=atof(word[6]);
				Y1=atof(word[7]);
				Z1=atof(word[8]);
				R1=atof(word[9]);
				Dx=X1-X0;
				Dy=Y1-Y0;
				Dz=Z1-Z0;


				/* first condition - not doing multicolour */
				if ( rcut1==-1000. || ( R1 < rcut2  && R1 > rcut1))
				{
					fprintf(output_wrl_file,"\n Translation { translation %8.3f%8.3f%8.3f }",Dx,Dy,Dz);
					fprintf(output_wrl_file,"\n	        Sphere { radius %8.3f }",R1);
					X0=X1; /*have output so store */
					Y0=Y1;
					Z0=Z1;
				}
			}
		}


	}/* end of reading input file */
	fprintf(output_wrl_file,"\n} # end of green spheres\n");
	/* rewind input file (i.e. go back to the first line */
	rewind( input_sphfile);

	/* third pass for blue spheres (over rcut2) */
	/* only do if doing different coloured spheres */
	if (rcut1 != -1000.)
	{
		X0 = 0.;
		Y0 = 0.;
		Z0 = 0.;
		fprintf( output_wrl_file,"Separator {\n");
		fprintf( output_wrl_file,"# blue spheres\n");
		fprintf( output_wrl_file,"Material { diffuseColor 0 0 1 # blue\n");
		fprintf( output_wrl_file,"           ambientColor 0 0 1 \n");
		fprintf( output_wrl_file,"           specularColor 0.800 0.800 0.800 \n");
		fprintf( output_wrl_file,"           shininess 0.750 \n");
		fprintf( output_wrl_file,"         }\n");

		while (fscanf(input_sphfile,"%s",word[0])!=EOF)
		{
			if (strcmp(word[0],"ATOM")==0)
			{
				for (i=1; i<11; i++)
				{
					fscanf(input_sphfile, "%s",word[i]);
				}
				if ( (strcmp(word[2],"QSS")!=0) || (strcmp(word[3],"SPH")!=0) )
				{
					/* ignore - warn on green */
				}
				else
				{
					X1=atof(word[6]);
					Y1=atof(word[7]);
					Z1=atof(word[8]);
					R1=atof(word[9]);
					Dx=X1-X0;
					Dy=Y1-Y0;
					Dz=Z1-Z0;
					/* radius must be above rcut2 for blue */
					if (  R1 > rcut2  )
					{
						fprintf(output_wrl_file,"\n Translation { translation %8.3f%8.3f%8.3f }",
						    Dx,Dy,Dz);
						fprintf(output_wrl_file,"\n	        Sphere { radius %8.3f }",R1);
						X0=X1; /*have output so store */
						Y0=Y1;
						Z0=Z1;
					}
				}
			}


		}
		/* end of reading input file */
		fprintf(output_wrl_file,"\n} # end of blue spheres\n");
		/* rewind input file (i.e. go back to the first line */
		rewind( input_sphfile);

	}

	/* final pass draw ends in grey */
	X0 = 0.;
	Y0 = 0.;
	Z0 = 0.;
	fprintf( output_wrl_file,"Separator {\n");
	fprintf( output_wrl_file,"# final ends to dark grey\n");
	fprintf( output_wrl_file,"Material { diffuseColor 0 0 0 # dark grey\n");
	fprintf( output_wrl_file,"         }\n");

	while (fscanf(input_sphfile,"%s",word[0])!=EOF)
	{
		if (strcmp(word[0],"ATOM")==0)
		{
			for (i=1; i<11; i++)
			{
				fscanf(input_sphfile, "%s",word[i]);
			}
			if ( (strcmp(word[2],"QSS")!=0) || (strcmp(word[3],"SPH")!=0) )
			{

			}
			else
			{
				X1=atof(word[6]);
				Y1=atof(word[7]);
				Z1=atof(word[8]);
				R1=atof(word[9]);
				/* simply store on read */
			}
		}
		else if (strcmp(word[0],"LAST-REC-END")==0)
		{
			/* previous record is an end draw in grey */
			Dx=X1-X0;
			Dy=Y1-Y0;
			Dz=Z1-Z0;
			fprintf(output_wrl_file,"\n Translation { translation %8.3f%8.3f%8.3f }",Dx,Dy,Dz);
			/* make sure radius slightly larger than original */
			fprintf(output_wrl_file,"\n	        Sphere { radius %8.3f }",R1+0.002);
			X0=X1; /*have output so store */
			Y0=Y1;
			Z0=Z1;

		}


	}/* end of reading input file */
	fprintf(output_wrl_file,"\n} # end of grey spheres\n");


	fclose(output_wrl_file); /* close output file */


	/*	system("rm final.wrl");
	system("cat temp.wrl temp2.wrl > final.wrl"); */
	/* run pdb2vrml */
	/*	system("pdb2vrml.irix53 -pdb pdb1grm.pdb temp2.wrl"); */

}
