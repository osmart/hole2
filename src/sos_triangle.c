/* now called sos_triangle ! */
/* surface: This  program is part of the HOLE suit of programs */
/* Copyright Guy M.P. Coates 1998-1999  */

/* Version 1.1 */
/* OSS 11.2000 introduce vmd option RENAME surface to sos_triangle */
/* OSS 11.2000 reorder_triangle function introduced to sort */
/*             out smoothed surface */
/* OSS 11.2000 introduce colour -1 dots for the ends - process these */
/*             in triangulation but cull these triangles before output */



/* This program generates a solid triangular mesh surface of a hole surface
in  various formats.
It uses a Step-by-step method of delaunay triangulation to calculate
the polygons:

Program reads a .sos file in ascii format from stdin  and writes
the surface file to  stdout */


#include <stdio.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>

/***************************************************************/
/*   Change the value of MAX_COORD in the following line to    */
/*   increase the number of polygons that can be accomodated.  */
/***************************************************************/

#define MAX_COORD 30000


/***************************************************************/
/*    Voodoo Angle: This angle defines the cutoff for the      */
/*    surface search: decrease this angle if the alogrithm     */
/*    forms closed surfaces where it shouldn't:                */
/***************************************************************/

#define VOODOO_ANGLE 102.0


/***************************************************************/
/*   You should not need to alter anything below this line!    */
/***************************************************************/

#define LINE_LEN 256  /* maximum line length */



/* Function declarations */

/* This structure is used for the tree of active edges */

  struct base_line {
    int a,b,c,z;   /* hold  coordinates of triangle and opposing vertex */ 
    
    /* flags to see if triangulation should continue from the edges */

    int base1_active,base2_active;
    
    /* pointers to daughter nodes */
    struct  base_line *base1;
    struct  base_line *base2;
  };

/* A linked list of edges: used to check if triangulation 
connects with previously triangulated area: contains pointers
to cross reference edges to entries in the tree */

struct edge_list {
    int x1,x2,*own_base,order;
    struct edge_list *next;
  };


void vrml_out();
void molscript_out();
void read_cord(); /* reads in the dots coordinates */
void polygonize(); /* generates the polygons */
int calc_tri(struct base_line *node);  /* calcs the triangles */

int check_point(int current_point); /* checks a point to see if it exists */
/* finds nearest neighbour */
int neighbour (struct base_line *node,double angle,double *min_dist);

/* writes triangle & calculates colour */
int gen_triangle (struct base_line *node);
/* adds list of edge to tree and linked list of edges */
int add_edge(int x1, int x2,int *own_base,int order);
/* Searches and removes edges from list and tree is the edges 
connect with other triangles */
void destroy (int edge1,int edge2, int *active);
/* writes out the bulk of VRML file */
void vrml_end();
/* converts quanta colours into RGB colour space */
void colour_conv(int col_index,double *red_ptr, double *green_ptr, double *blue_ptr);
/* procedure to cull zero area triangle */
void cull_triangles();
/* compares two vectors to see if they are the same */
int vec_compare (int vec_a,int vec_b);
/* cull duplicate coordinates */
void cull_coords();
/* find the normal xyz to triangle abc */
int tri_normal (int a,int b,int c,double *x_ptr,double *y_ptr,double *z_ptr);
/* Function to output in prepi free file format */
void prepi_out();
/* calculate the normals at all the vertices */
int vertex_normals();
/* check for back faced polygons */
int back_check(int a);

void povray_out();
void help();

/* OSS 11-2000 vmd output option*/
void vmd_out();
/* OSS 11-2000 reorder_triangle needed to sort out smoothed surfaces*/
void reorder_triangle();

/* Global Variables */

double dots[MAX_COORD][7]; /* hold the xcoor:ycoor:zcoor:colour:nx:ny:nz records of the points */
int tri[MAX_COORD][5]; /* list of polygons: dot1:dot2:dot3:colour:flipped */
int culled_tri[MAX_COORD][6];
double in_dots[MAX_COORD][7]; /* inital dots read in from HOLE: contains redundant coords*/
double triangle_normals[MAX_COORD][3]; /* holds normals to the triangles */


int in_dots_total=0;
int max_dots=0;  /* the total number of points */
int tri_count=0; /* counter for polygons */
int culled_tri_count=0;
struct base_line *root;  /* first base line */
struct edge_list *start; /* start and end of linked list of edges */
struct edge_list *end;

int smooth=0; /* flag for smoothed surfaces */
int dump=0; /* falg for dumping colour records */
int start_point=0; /* contains the starting point */
int flipped=0;
int format=4; /* OSS vmd now the default */
double axis[3];
float max_vertex_length=5.0; /* maximum length for a triangle vertex */

int main (int argc, char *argv[])
{
  
  int exists;
  int current_point;
  int loop1,loop2;
  
  
  
  fprintf (stderr,"sos_triangle: A Hole surface generation program\n");
  fprintf (stderr,"Copyright 1997-9 Guy M.P. Coates \n");
  fprintf (stderr,"Copyright 2000, 2004 Oliver S. Smart \n"); 
  fprintf (stderr,"Copyright 2014-2015 SmartSci Limited, All rights reserved.\n"); 
  fprintf (stderr,"For help on HOLE suite see  http://www.smartsci.uk/hole/\n");
  
  /* initialize the arrays.... */
  
  for (loop1=0;loop1<MAX_COORD;loop1++)
    {
      for (loop2=0;loop2<4;loop2++)
	{
	  dots[loop1][loop2]=-1;
	  tri[loop1][loop2]=-1;
	}
    }


  
  
  /* parse the command line options */

  /*  two options -h for help and -smooth for smooth surfaces -n for number*/
  /* add other options for molscript output */

  while ((argc>1) && (argv[1][0] == '-'))
	
    {
      switch (argv[1][1])
	{
	  
	  /* -h  help!*/
	case 'h':
	  
	  help();
	  return(0);
	  
	case 's':
	  smooth=1;
	  fprintf (stderr,"\nProducing Smooth surface (buggy!).");
	  break;
	  
	case 'm':
	  format=1;
	  fprintf (stderr,"\nProducing Molscript surface.");
	  break;
	  
	case 'l':
	  format=0;
	  fprintf (stderr,"\nProducing VRML surface.");
	  break;
	  
	case 'p':
	  format=2;
	  fprintf (stderr,"\nProducing Prepi surface.");
	  break;
	  
	case 'r':
	  format=3;
	  fprintf (stderr,"\nProducing Povray surface.");
	  break;
	  
       
	case 'd':
	  dump=1;
	  fprintf (stderr,"\nDumping colour records for molscript.");
	  break;

        /* OSS 11/00 vmd surface */
	case 'v':
	  format=4;
	  fprintf (stderr,"\nProducing vmd surface");
	  break;

        case 'X':
	  /* OSS 11/00 maximum distance for a triangle for output */
	  /* pickup number from next arg */
          sscanf(argv[2],"%f",&max_vertex_length);
	  argc--;
          argv++; /* ignore next arg */
	  /*
	  fprintf(stderr,"\nDistance stuff");	
          fprintf(stderr,"\n next arg= `%s`",argv[2]);	    
 	  fprintf(stderr,"\n max_vertex_length= %f",max_vertex_length);
	  */
	  break;
	default:
	  help();
	  return(0);

	}
      
      
      argc--;
      argv++;
    }
  
  
  if (smooth==0)
    {
      fprintf(stderr,"\nProducing Faceted surface.");
    }
  
  /* read in data and purge dupliacte coordinates */
  
  read_cord();
  cull_coords();

   
  
  /* each point is chosen: if is has been incorporated into
     a triangle it is skipped over; if not it is used 
     as the start point for the next polyonisation. */

  exists=0;
  start_point=0;
  current_point=0;

  /* generate start of linked lists for lists of edges and pointers... */
  
  start=malloc(sizeof(struct edge_list)); /* generate start of list */
  end=start;

  fprintf (stderr,"\nGenerating surface: This could take some time....");

  
  for (current_point=0;current_point<max_dots;current_point++)
    {

      /* check to see if point has been incorporated into the surface yet :
	 if it has not use it as the starting point to generate the surface from */

      if (check_point(current_point)==0)
	{
	  start_point=current_point;
	  polygonize();
	}
    }

  fprintf (stderr,"\nNumber of polygons: %i",tri_count);


  
  /* remove redundant triangles */

  
  cull_triangles();




 /* generate the normals for the polygons: */

  /* This step is now done in the sph-->sos conversion: The new HOLE algorithm
 broke it though. This code may be reintroduced is further releases:  */


  /*

  fprintf (stderr,"\nCalculating Normals to the triangles.\n");
  
  
  
  for (loop1=0;loop1<culled_tri_count;loop1++)
    {
      tri_normal(culled_tri[loop1][0],culled_tri[loop1][1],culled_tri[loop1][2],
		 &triangle_normals[loop1][0],
		 &triangle_normals[loop1][1],
		 &triangle_normals[loop1][2]);
    }
    
    */

  /* find incorrectly facing polygons and flip them around */
  /* set the normal of the first triangle as the default direction */

/*  
  for (loop1=0;loop1<culled_tri_count;loop1++)
    {
      
      back_check(loop1);
    }
  
  
  fflush (stderr);
 */ 
  
  /* if producing smooth surface reorder triangles as necessary*/		 
  if (smooth==1)
    {
    reorder_triangle();
    }

		 
		   
  /* write out the surface in the appropriate format */
  
  
  switch (format)
    
    {
    case 0:
      fprintf (stderr,"\nWriting VRML world file:");
      vrml_out();
      vrml_end ();
      break;
  
    case 1:
      fprintf (stderr,"\nWriting Molscript object file:");
      molscript_out();
      break;
      
    case 2:
      fprintf (stderr,"\nWriting Prepi object file:");
      prepi_out();
      break;
      
    case 3:
      fprintf (stderr,"\nWriting Povray file:");
      povray_out();
      break;
      
    case 4:
      fprintf (stderr,"\nWriting vmd file:");
      vmd_out();
      break;
    }
  
  fprintf (stderr,"\nProgram COMPLETE\n\b\b");
  return (0);
  
}

void read_cord()

{

  int total_dots=0;
  float num1,num2,num3,num4,num5,num6,num7;
  char line_in[LINE_LEN];
  float colour=1.000;

  /* gets the coordinates from stdin */

  fprintf (stderr,"\nWaiting for  coordinates:");
  
  while (fgets(line_in,LINE_LEN,stdin)!=NULL)
    {
      sscanf(line_in,"%f%f%f%f%f%f%f",&num1,&num2,&num3,&num4,&num5,&num6,&num7);
      
      /* what happens next depends on the first number */
      /* if num1 =4 then the line is a point */
      /* if num1 =3 then its a change in colour */
      /* colour selections are hard coded for now ...*/

      if (num1==1.000000)
	{
	  colour=num2;
	  continue;
	}


      if (num1==4.000000)
	{
	 
	  if (total_dots==MAX_COORD)
	    {
	      fprintf (stderr,"\n\n\b\b\bERROR:Maximum number of co-ordinates exceeded!");
	      fprintf (stderr,"\nProgram TERMINATED\n");
	      exit (1);
	    }
	  

	  in_dots[total_dots][0]=num2;
	  in_dots[total_dots][1]=num3;
	  in_dots[total_dots][2]=num4;
	  in_dots[total_dots][3]=colour;
	  in_dots[total_dots][4]=num5;
	  in_dots[total_dots][5]=num6;
	  in_dots[total_dots][6]=num7;
	  total_dots++;
	}
    }
  
  /* max_dots is the total number of dots; GLOBAL variable used all
     over the place */
  
  in_dots_total=total_dots;
  fprintf (stderr,"\n%i coordinates read.",in_dots_total);
  return;
    
}

void polygonize()
{
  
  int initb;
  double distance,mindistance;
  int loop;
  int dummy;
  int first_loop_flag=0;
  
 

  /* find the nearest neigbour to the first dot */

  for (loop=0;loop<max_dots;loop++)
    {
      if (loop==start_point)
	continue;
      
      distance=(pow(dots[loop][0]-dots[start_point][0],2)+
		pow(dots[loop][1]-dots[start_point][1],2)+
		pow(dots[loop][2]-dots[start_point][2],2));
      if ((distance < mindistance) || (first_loop_flag==0))
	{
	  first_loop_flag=1;
	  mindistance=distance;
	  initb=loop;
	}
    }
  
  /* add the first baseline to the root node in the tree */

  root=malloc(sizeof(struct base_line));
  root->a=start_point;
  root->b=initb;
  root->z=initb;
  root->c=0;
  root->base1=NULL;
  root->base2=NULL;
  root->base1_active=1;
  root->base2_active=1;
  
  /* add baseline to the edge list */

  if (end==start)
    {
      /* must be the first entry in the list...*/
      end->next=NULL;
      end->x1=start_point;
      end->x2=initb;
      end->own_base=&dummy;
      end->order=2;
    }
  else
    {
      end->next=malloc(sizeof(struct edge_list));
      end=end->next;
      end->next=NULL;
      end->x1=start_point;
      end->x2=initb;
      end->own_base=&dummy;
      end->order=2;
    }
  calc_tri(root);
	      
return;

}
  
int calc_tri(struct base_line *node)

{
  
  double max_angle;
  double prev_dist;
  node->c=(max_dots+1);
  max_angle=cos( VOODOO_ANGLE ); /* defines cone of space to search for thessian neighbour:
		     value is cos, so time consuming acos need not be calculated */

  /* find the thessian neighbour */

  node->c=neighbour(node,max_angle,&prev_dist); 
 
  /* if returned values is = max_dots+1 it means no neighbour
     has been found; must be at a boundry */

  if (node->c==(max_dots+1))
    {
      return(0);
    }
    
 
  
  gen_triangle(node);
  

  /* search to see if 2 new edges have connected with
     a previous edge.*/

  destroy(node->a,node->c,&node->base1_active);
  destroy(node->b,node->c,&node->base2_active);
  
  
  /* If the edge had not connected, add it to the list 
     and generate a triangle from it */
  

  if ((node->base2_active)==1)
    {
      add_edge(node->b,node->c,&node->base2_active,2); 
    }     
  
  
  if ((node->base1_active)==1)
    {
      add_edge(node->a,node->c,&node->base1_active,1); 
      node->base1=malloc(sizeof(struct base_line));
      node->base1->a=node->a;
      node->base1->b=node->c;
      node->base1->z=node->b;
      node->base1->c=0;
      node->base1->base1_active=1;
      node->base1->base2_active=1;
      node->base1->base2=NULL;
      node->base1->base1=NULL;
      calc_tri(node->base1);
    }
  
  /* Do the same for the second edge */
  /* (check is needed here due to recursive nature of function )*/
    
  if ((node->base2_active)==1)
    {
      node->base2=malloc(sizeof(struct base_line));
      node->base2->a=node->c;
      node->base2->b=node->b;
      node->base2->z=node->a;
      node->base2->c=0;
      node->base2->base1_active=1;
      node->base2->base2_active=1;
      node->base2->base1=NULL;
      node->base2->base2=NULL;
      calc_tri(node->base2);
    }
  
  
  return(0);
 
}

/* find nearest neighbour */
/* Neighbour C is point which subtends greatest angle */
/* with the baseline AB */

int neighbour (struct base_line *node, double angle_limit, double *prev_dist)
{
  double veca[3];  /* vector for a->c */
  double vecb[3];  /* vector for b->c  */
  double pointm[3];  /* midpoint a-b */
  double veczm[3]; /* vector for z-> m */
  double vecmc[3]; /* vector for z->c */
  double maga,magb,dotp,magmc,magzm;
  double angle,angle2,min_ang=1.0;
  int c=(max_dots+1);
  int loop;
  double check_dist;
  double base_dist;


  /* search through all dots for neigbours. (slow, but works...) */
  /* Box type data structure would be better... */

  for (loop=0;loop<max_dots;loop++)
    {
      if ((loop==node->a) || (loop==node->b) || (loop==node->z))
	continue;
      
      /* define two vectors A (a->c) and B (b->c) */
      
      pointm[0]=0.5*(dots[node->a][0]+dots[node->b][0]);
      pointm[1]=0.5*(dots[node->a][1]+dots[node->b][1]);
      pointm[2]=0.5*(dots[node->a][2]+dots[node->b][2]);
      veczm[0]=pointm[0]-dots[node->z][0];
      veczm[1]=pointm[1]-dots[node->z][1];
      veczm[2]=pointm[2]-dots[node->z][2];
      vecmc[0]=dots[loop][0]-pointm[0];
      vecmc[1]=dots[loop][1]-pointm[1];
      vecmc[2]=dots[loop][2]-pointm[2];
      

      /* See if dot is within allowed arc of space:*/

      magmc=sqrt(pow(vecmc[0],2)+pow(vecmc[1],2)+pow(vecmc[2],2));
      magzm=sqrt(pow(veczm[0],2)+pow(veczm[1],2)+pow(veczm[2],2));
      dotp=(vecmc[0]*veczm[0])+(vecmc[1]*veczm[1])+(vecmc[2]*veczm[2]);
      angle2=((dotp/(magmc*magzm)));
      
      

      /* if dot is in the cone of space, calc the angle it subtends 
	 to the baseline */
      
      if (angle2 > angle_limit)
	{
	  
	  veca[0]=dots[loop][0]-dots[node->a][0];
	  veca[1]=dots[loop][1]-dots[node->a][1];
	  veca[2]=dots[loop][2]-dots[node->a][2];
	  vecb[0]=dots[loop][0]-dots[node->b][0];
	  vecb[1]=dots[loop][1]-dots[node->b][1];
	  vecb[2]=dots[loop][2]-dots[node->b][2];
	  maga=sqrt(pow(veca[0],2)+pow(veca[1],2)+pow(veca[2],2));
	  magb=sqrt(pow(vecb[0],2)+pow(vecb[1],2)+pow(vecb[2],2));
	  dotp=(veca[0]*vecb[0])+(veca[1]*vecb[1])+(veca[2]*vecb[2]);
	  angle=(dotp/(maga*magb));
	  

	  /* Kludge  check: unknown bug sometimes causes long
	     and thin triangle to be generated for some reason:
	     routine gets rid of triangles if length of triangle
	     is 3 times length of base...*/


	  
	  if ((angle < min_ang))  
 	    {
	      check_dist=(pow(dots[loop][0]-dots[node->a][0],2)+  
  			  pow(dots[loop][1]-dots[node->a][1],2)+  
  			  pow(dots[loop][2]-dots[node->a][2],2)); 
  	      base_dist=(pow(dots[node->a][0]-dots[node->b][0],2)+
  			 pow(dots[node->a][1]-dots[node->b][1],2)+ 
  			 pow(dots[node->a][2]-dots[node->b][2],2));  
	      
 	      if (check_dist>(9*base_dist))  
 		continue;  
	      
	      
	      /*returns the thessian neighbour...*/
	      
	      min_ang=angle;
	      c=loop; 

	    } 
	  
	}
    } 
  
  return (c);
}



int gen_triangle (struct base_line *node)

{

  /* writes triangle to array of generated triangles */


  if (tri_count==MAX_COORD)
    {
      fprintf (stderr,"\nERROR: Maximum number of polygons exceeded!");
      fprintf (stderr,"\nProgram TERMINATED\n");
      exit(1);
    }


  tri[tri_count][0]=node->a;
  tri[tri_count][1]=node->b;
  tri[tri_count][2]=node->c;

  /* work out colour */

  /* if two or more of the dots have the same colour make the triangle 
that colour */
  
  if ((dots[node->a][3]==dots[node->b][3]) ||
      (dots[node->a][3]==dots[node->c][3]))
    {
    tri[tri_count][3]=(int)(dots[node->a][3]);
    tri_count++;
    return(0);
    }

  if (dots[node->b][3]==dots[node->c][3])
    {
      tri[tri_count][3]=(int)(dots[node->b][3]);
      tri_count++;
      return(0);
    }
  /* if each dot is different assign it A's colour*/
  
  tri[tri_count][3]=(int)(dots[node->a][3]);
  tri_count++;
  return(0);
			  
}

  
/* function to add edge to list of edges  */

int add_edge(int x1, int x2,int *own_base,int order)

{
  end->next=malloc(sizeof(struct edge_list));
  end=end->next;
  end->x1=x1;
  end->x2=x2;
  end->order=order;
  end->own_base=own_base;
  end->next=NULL;
  return(0);
}


void vrml_out()
{
  
  /* writes out the start of the VRML file */
  int loop=0;

  /* VRML header */
  /* required for all VRML files */

  printf ("#VRML V1.0 ascii");

  /* Add some shapehints to help the renders render the shapes */

  printf ("\nSeparator {");
  printf ("\n Separator{");
  printf ("\n ShapeHints \n {");
  printf ("\n vertexOrdering COUNTERCLOCKWISE ");
  printf ("\n shapeType UNKNOWN_SHAPE_TYPE");
 
  /* turn on or off the smooth or faceted surface */

  if (smooth==0)
    printf ("\n creaseAngle 0");
  else
    printf ("\n creaseAngle 2");
  
  
  printf ("\n faceType CONVEX\n }\n");
  printf ("\nCoordinate3 { \n   point [\n");
  
  /* write out all the raw Coordinates  (VRML PointSet field)*/

  for (loop=0;loop<max_dots;loop++)
    {
      /* each line has to end with a , except the last one */
      if (loop!=0)
	{
	  printf(",\n");
	}
      
      printf ("%f   %f   %f",dots[loop][0],dots[loop][1],dots[loop][2]);
    }
  printf("\n");
  printf("] \n }\n");
  /*  printf("PointSet {\n");
  printf("startIndex 0 \n numPoints -1 \n } \n ");*/
  
  return;
}

void vrml_end()
{
  
  /* writes out the generated polygons: All triangles of the same
     colour are written out together. */


  int loop_cntr;
  int first_polygon=0;
  
  /* print out red spheres */
  /* write out the material description for a shiny red */

  printf ("\n Separator { \n");
  printf ("Material { \n");
  printf ("diffuseColor 0.8 0.0 0.0\n ");
  printf ("specularColor 1.0 1.0 1.0\n");
  printf ("ambientColor 0.4 0.0 0.0 \n");
  printf ("shininess 0.5");
  printf ("}\n");
  
  /* writes out the connectivites of the red triangles */
  /* ( VRML IndexedFaceSet field) */

  printf ("\nIndexedFaceSet { \n");
  printf ("coordIndex [ \n");
  
  for (loop_cntr=0;loop_cntr<culled_tri_count;loop_cntr++)
    {
      if (culled_tri[loop_cntr][3]==3)
	{
	  if (first_polygon!=0)
	    {
	      printf(",\n");
	    }
	  printf ("%i, %i, %i, -1",culled_tri[loop_cntr][0],culled_tri[loop_cntr][1],
		  culled_tri[loop_cntr][2]);
	  first_polygon=1;
	}
    }
  printf ("\n]\n}\n}");

  /* now print out the green spheres */

 printf ("\n Separator { \n");
  printf ("Material { \n");
  printf ("diffuseColor 0.0 0.8 0.0\n ");
  printf ("ambientColor 0.0 0.4 0.0\n");
  printf ("specularColor 1.0 1.0 1.0 \n");
  printf ("shininess 0.5");
  printf ("}\n");

  printf ("\nIndexedFaceSet { \n");
  printf ("coordIndex [ \n");
  
  first_polygon=0;
  for (loop_cntr=0;loop_cntr<culled_tri_count;loop_cntr++)
    {
      if (culled_tri[loop_cntr][3]==7)
	{
	  if (first_polygon!=0)
	    printf(",\n");
	  
	  first_polygon=1;
	  printf ("%i, %i, %i, -1",culled_tri[loop_cntr][0],culled_tri[loop_cntr][1],
		  culled_tri[loop_cntr][2]);
	}
    }
  printf ("\n]\n}\n}");

  /* ...and finally print out blue spheres */

printf ("\n Separator { \n");
  printf ("Material { \n");
  printf ("diffuseColor 0.0 0.0 0.8\n ");
  printf ("ambientColor 0.0 0.0 0.4\n ");
  printf ("specularColor 1.0 1.0 1.0 \n");
  printf ("shininess 0.5 \n");
  printf ("}\n");

  printf ("\nIndexedFaceSet { \n");
  printf ("coordIndex [ \n");
  
  first_polygon=0;
  for (loop_cntr=0;loop_cntr<culled_tri_count;loop_cntr++)
    {
      if (culled_tri[loop_cntr][3]==2)
	{
	  if (first_polygon!=0)
	    printf(",\n");
	  
	  printf ("%i, %i, %i, -1",culled_tri[loop_cntr][0],culled_tri[loop_cntr][1],
		  culled_tri[loop_cntr][2]);
	  first_polygon=1;
	}
    }
  printf ("\n]\n}\n}");
  printf ("\n}\n}");
  return;
}



void destroy (int edge1,int edge2, int *active)

{
  struct edge_list *list_ptr;
  struct edge_list *prv_ptr;
  
  list_ptr=start;
  prv_ptr=start;
  
  /* go thorugh the list the see if edges have already been
     generated */


  while (list_ptr!=NULL)
    {
     
      
      
      if (((edge1==list_ptr->x1) && (edge2==list_ptr->x2)) ||
	  ((edge1==list_ptr->x2) && (edge2==list_ptr->x1)))
	{
	  
	  /* if edge is the same both connecting edges must be
	     flagged as being connected: the active flag in 
	     the edge tree is voided to signal triangulation
	     must not continue */

	  *active=0;
	  *(list_ptr->own_base)=0;
	  list_ptr->order=1;
	  return;
	}
      
      prv_ptr=list_ptr;
      list_ptr=list_ptr->next;
      
    }
  return;
}


int check_point (int current_point)
{

  int cntr1,cntr2;

  for (cntr1=0;cntr1<tri_count;cntr1++)
    {
      for (cntr2=0;cntr2<3;cntr2++)
	{
	  if (current_point==tri[cntr1][cntr2])
	    return(1);
	}
    }
  return(0);
}
	    

void molscript_out ()
{
  
  int loop_cntr=0;
  int tri=0;
  double red,green,blue;

  
  /* Print total number of triangles */
  if (smooth==0 && dump==0 )
    {
      fprintf (stdout,"TC %i\n",(culled_tri_count*3));
    }
 
  if (smooth==1 && dump==0)
    {
      /* vertex_normals(); */
      fprintf (stdout,"TNC %i\n",(culled_tri_count*3));
    }
  if (dump==1 && smooth==0)
    {
      fprintf (stdout,"T %i \n",(culled_tri_count*3));
    }
  if (dump==1 && smooth==1 )
    {
      fprintf (stdout,"TN %i \n",(culled_tri_count*3));
    }


  /* output the coordinates for each triangle */

 for (loop_cntr=0;loop_cntr<culled_tri_count;loop_cntr++)
   {
   
     for (tri=0;tri<3;tri++)
       {
	 /* print out the coordinates */
	 
	 
	 fprintf (stdout,"%f %f %f ",
		  dots[culled_tri[loop_cntr][tri]][0],
		  dots[culled_tri[loop_cntr][tri]][1],
		  dots[culled_tri[loop_cntr][tri]][2]);
	 
	 /*do we need normals? */
	 
	 if (smooth==1)
	   {
	     fprintf (stdout,"%f %f %f ",
		      dots[culled_tri[loop_cntr][tri]][4],
		      dots[culled_tri[loop_cntr][tri]][5],
		      dots[culled_tri[loop_cntr][tri]][6]);
	   }
	 
	 /*Do we need colours? */
	 
	 if (dump==0)
	   {
	 colour_conv  (dots[culled_tri[loop_cntr][tri]][3],&red,&green,&blue);
	 fprintf (stdout, "%f %f %f ",
		  red,green,blue);
	   }
	 
     
     /* Go onto the next line... */
     fprintf (stdout, " \n");
       }
   }

 fprintf (stdout,"Q\n");
 return;
 
 
}
	
 	

void colour_conv(int col_index,double *red_ptr, double *green_ptr, double *blue_ptr)  

{
  
  switch (col_index)
    {
    case 2:
     /* blue dot */
      *red_ptr=0.0;
      *green_ptr=0.0;
      *blue_ptr=1.0;
      break;


    case 3:
      /* red dot */
      *red_ptr=1.0;
      *green_ptr=0.0;
      *blue_ptr=0.0;
      break;


    case 7:
      /* green dot */
      *red_ptr=0.0;
      *green_ptr=1.0;
      *blue_ptr=0.0;
      break;
      
    default:
      /* this should never happen! */
      *red_ptr=0.5;
      *green_ptr=0.5;
      *blue_ptr=0.5;
    }
  return;
}

void cull_triangles () 
     
{ 
  int loop_cntr=0; 
  int cull_end_count=0,cull_big_count=0; 
  int dlistA, dlistB, dlistC;
  double lenAB, lenAC, lenCB, max_v_sq;
  
   /* the maximum length squared */  
   max_v_sq = max_vertex_length*max_vertex_length;
  
   /*   Check To See If The Triangles Have Zero Area: Cull Them If  So */ 
   
   for (loop_cntr=0;loop_cntr<tri_count;loop_cntr++) 
     { 
   
       /*      Compare A And B Coordinate */
       /* CHECK NOT NEEDED: THIS BLOCK INTENTIONALLY COMMENTED OUT! */
       /* We now cull duplicate coordinates instead: The hole surface can produce */
       /* two points with the same coordiantes. This breaks the algorithm in intersting */
       /* ways.... */
       /*if (  
	 
	   vec_compare(tri[loop_cntr][0],tri[loop_cntr][1]) ||  
 	   vec_compare(tri[loop_cntr][0],tri[loop_cntr][2]) ||  
 	   vec_compare(tri[loop_cntr][2],tri[loop_cntr][1]))  
	 
	{  
 	   cull_count++; 
           continue; 
 	} */
	
	/* check the length**2 of AB AC CB */
        /* find A B C indices */
        dlistA = tri[loop_cntr][0];
	dlistB = tri[loop_cntr][1];
	dlistC = tri[loop_cntr][2];
        /* length**2 */
	lenAB = (dots[dlistB][0]-dots[dlistA][0])*(dots[dlistB][0]-dots[dlistA][0])+
 	        (dots[dlistB][1]-dots[dlistA][1])*(dots[dlistB][1]-dots[dlistA][1])+
		(dots[dlistB][2]-dots[dlistA][2])*(dots[dlistB][2]-dots[dlistA][2]);
	
	lenAC = (dots[dlistC][0]-dots[dlistA][0])*(dots[dlistC][0]-dots[dlistA][0])+
 	        (dots[dlistC][1]-dots[dlistA][1])*(dots[dlistC][1]-dots[dlistA][1])+
		(dots[dlistC][2]-dots[dlistA][2])*(dots[dlistC][2]-dots[dlistA][2]);
	
	lenCB = (dots[dlistB][0]-dots[dlistC][0])*(dots[dlistB][0]-dots[dlistC][0])+
 	        (dots[dlistB][1]-dots[dlistC][1])*(dots[dlistB][1]-dots[dlistC][1])+
		(dots[dlistB][2]-dots[dlistC][2])*(dots[dlistB][2]-dots[dlistC][2]);
		
	/* 11-2000 new use for this loop */
	/* check for end records - colour -1 */	 			
        /* vble dots[MAX_COORD][7]; holds the  */
	/*      xcoor:ycoor:zcoor:colour:nx:ny:nz records of the points */     
        /* i.e. color is in dots[blah][3] where blah is a list number */
        /* list numbers for each triangle are */
        /* tri[loop_cntr][0] and tri[loop_cntr][1] and tri[loop_cntr][2]*/
	if ( dots[tri[loop_cntr][0]][3]<0 ||
	     dots[tri[loop_cntr][1]][3]<0 ||
	     dots[tri[loop_cntr][2]][3]<0 		
	   ) 
	  { /* triangle rejected */
 	   cull_end_count++; 
           continue; 
	  }
	else 
	  {
	  if ( (lenAB > max_v_sq) || (lenAC > max_v_sq) || (lenCB > max_v_sq) )
	    { /* one of the triangle vertices over 5 angs */
	    cull_big_count++;
	    continue; 
	    }
	  else
	    {  /* triangle accepted */		
            culled_tri[culled_tri_count][0]=tri[loop_cntr][0];
       	    culled_tri[culled_tri_count][1]=tri[loop_cntr][1];
       	    culled_tri[culled_tri_count][2]=tri[loop_cntr][2];  
       	    culled_tri[culled_tri_count][3]=tri[loop_cntr][3];
       	    culled_tri[culled_tri_count][4]=0;
       	    culled_tri_count++;
            }
	  }
   } 
   fprintf(stderr,"\n%i End triangles culled.",cull_end_count); 
   fprintf(stderr,"\n%i Triangles culled as a vertex greater than %f angs",cull_big_count,max_vertex_length); 
   fprintf(stderr,"\n%i polgons remaining.",culled_tri_count);
   return; 
} 

int vec_compare (int vec_a,int vec_b)
{
  
  
  if (
      ((fabs(in_dots[vec_a][0]- dots[vec_b][0]))<(1.0E-3)) &&
      ((fabs(in_dots[vec_a][1]- dots[vec_b][1]))<(1.0E-3)) &&
      ((fabs(in_dots[vec_a][2]- dots[vec_b][2]))<(1.0E-3))
      )

    {
      return(1);
    }
  
  
  return(0);
}

void cull_coords()
{
  int in_cntr;
  int cull_cntr;
  int duplicate_flag=0;
  int cull_counter=0;

  fprintf (stderr,"\nCulling duplicate coordinates.");

  for (in_cntr=0;in_cntr<in_dots_total;in_cntr++)
    {
      
	
      for (cull_cntr=0;cull_cntr<max_dots;cull_cntr++)
	{
		  
	  if 
	    (vec_compare(in_cntr,cull_cntr)==1)
	    {
	      duplicate_flag=1;
	      break;
	    }
	}

    
      if (duplicate_flag!=1)
	{
	  dots[max_dots][0]=in_dots[in_cntr][0];
	  dots[max_dots][1]=in_dots[in_cntr][1];
	  dots[max_dots][2]=in_dots[in_cntr][2];
	  dots[max_dots][3]=in_dots[in_cntr][3];
	  dots[max_dots][4]=in_dots[in_cntr][4];
	  dots[max_dots][5]=in_dots[in_cntr][5];
	  dots[max_dots][6]=in_dots[in_cntr][6];

	  max_dots++;
	}
      else
	{
	  cull_counter++;
	}
      duplicate_flag=0;
    }

  fprintf (stderr,"\n%i coordinates removed.",cull_counter);
  fprintf (stderr, "\n%i coordinates remaining.",max_dots);

  return;

}

int tri_normal (int a,int b,int c,double *x_ptr, double *y_ptr, double *z_ptr)
{
  
  
  

  double AB[3];
  double AC[3];

  double normal[3];
  double crossp[3];
  double  mag;
  double vector;


  return (0);

  /* generate two vectors, AB and AC */

  AB[0]=dots[b][0]-dots[a][0];
  AB[1]=dots[b][1]-dots[a][1];
  AB[2]=dots[b][2]-dots[a][2];
  
  AC[0]=dots[c][0]-dots[a][0];
  AC[1]=dots[c][1]-dots[a][1];
  AC[2]=dots[c][2]-dots[a][2];

  /* normal is the crosss  product of the two vectors */
  
  crossp[0]=(AB[1]*AC[2])-(AB[2]*AC[1]);
  crossp[1]=(AB[2]*AC[0])-(AB[0]*AC[2]);
  crossp[2]=(AB[0]*AC[1])-(AB[1]*AC[0]);

  /* convert normal to a unit vector */

  mag=sqrt (pow(crossp[0],2)+pow(crossp[1],2)+pow(crossp[2],2));
  normal[0]=crossp[0]/mag;
  normal[1]=crossp[1]/mag;
  normal[2]=crossp[2]/mag;

  
  *x_ptr=(float)normal[0];
  *y_ptr=(float)normal[1];
  *z_ptr=(float)normal[2];
  
  return(0);
}

/* prepi out: Write prepi surface format */

void prepi_out ()
{

  int loop_cntr;
  
  if (smooth==1)
    {
      fprintf (stderr,"\b\b\nWARNING: Smooth surface option not available in Prepi format!\n");
      fprintf (stderr,"The colour records may be ignored in some versions of Prepi. \n");
    }

  /* vertex normals needed */
  vertex_normals();
  
  fprintf (stdout,"saisurpol\n");
   
  for (loop_cntr=0;loop_cntr<culled_tri_count;loop_cntr++)
   {
     
     
     /* Print out the prepi surface: V1 x y z V2 x y z V3 x y z V1 Nx Ny Nz etc */     
     
     fprintf (stdout,"%6.1f%6.1f%6.1f",
	      dots[culled_tri[loop_cntr][0]][0],
	      dots[culled_tri[loop_cntr][0]][1],
	      dots[culled_tri[loop_cntr][0]][2]);
     
     fprintf (stdout,"%6.1f%6.1f%6.1f",
	      dots[culled_tri[loop_cntr][1]][0],
	      dots[culled_tri[loop_cntr][1]][1],
	      dots[culled_tri[loop_cntr][1]][2]);

     fprintf (stdout,"%6.1f%6.1f%6.1f",
	      dots[culled_tri[loop_cntr][2]][0],
	      dots[culled_tri[loop_cntr][2]][1],
	      dots[culled_tri[loop_cntr][2]][2]);

     fprintf (stdout,"%6.1f%6.1f%6.1f",
	      dots[culled_tri[loop_cntr][0]][4],
	      dots[culled_tri[loop_cntr][0]][5],
	      dots[culled_tri[loop_cntr][0]][6]);

     fprintf (stdout,"%6.1f%6.1f%6.1f",
	      dots[culled_tri[loop_cntr][1]][4],
	      dots[culled_tri[loop_cntr][1]][5],
	      dots[culled_tri[loop_cntr][1]][6]);

     fprintf (stdout,"%6.1f%6.1f%6.1f\n",
	      dots[culled_tri[loop_cntr][2]][4],
	      dots[culled_tri[loop_cntr][2]][5],
	      dots[culled_tri[loop_cntr][2]][6]);
     
     
   }

}
  
int vertex_normals()

{
  
  
  

  
  double nx=0.0;
  double ny=0.0;
  double nz=0.0;
  int normal_count=0;
  int loop_cntr;
  int loop_cntr2;
  double dotp;
  double vector=0.0;
  
  return(0);

  fprintf (stderr,"\ncalculating Normals at the vertices.\n");


  /* calculate average normal at each vertex */

  /* check each point in turn: average the triangle normals of every triangle it forms
     a vertex to. */
  
  for (loop_cntr=0;loop_cntr<max_dots;loop_cntr++)
    {
      for (loop_cntr2=0;loop_cntr2<culled_tri_count;loop_cntr2++)
	{

	  if ((culled_tri[loop_cntr2][0]==loop_cntr) ||
	      (culled_tri[loop_cntr2][1]==loop_cntr) ||
	      (culled_tri[loop_cntr2][2]==loop_cntr))

	    {
	     nx+=triangle_normals[loop_cntr2][0];
	     ny+=triangle_normals[loop_cntr2][1];
	     nz+=triangle_normals[loop_cntr2][2];
	     normal_count++;
	    }
	}

      

      dots[loop_cntr][4]=(nx/normal_count);
      dots[loop_cntr][5]=(ny/normal_count);
      dots[loop_cntr][6]=(nz/normal_count);
      
      nx=0;
      ny=0;
      nz=0;
      normal_count=0;
    }
  

  return(0);
}

int back_check (int a)
{
  



  int loop1,loop2;
  int tri_neigh[100]; /* CULLED_TRI index */
  int hit=0;
  int cntr1;
  int ncount=0;
  double dotp;
  int old;

  return(0);

  if (culled_tri[a][5]==1)
    return(0);
  

  /* find adjoining traingles */
  

  /* go through all triangles and find number of shared vertices: if the traingles have
     two shared vertices then they are neighbours */

  for (loop1=0;loop1<culled_tri_count;loop1++)
    {

      if (loop1==a)
	continue;

      for (loop2=0;loop2<3;loop2++)
	{
	  if ((culled_tri[loop1][loop2]==culled_tri[a][0]) || 
	      (culled_tri[loop1][loop2]==culled_tri[a][1]) || 
	      (culled_tri[loop1][loop2]==culled_tri[a][2]))
	    {
	      hit++;
	    }
	}
      
      if (hit==2)
	{
	  if (ncount<3)
	    {
	      tri_neigh[ncount]=loop1;
	      ncount++;
	    }
	}

      hit=0;
    }
  


  /* compare the normals of the neighbouring traingles to A: if the dotp of the 
     triangles is < 0 then the normals are running anti parallel to one another and the
     triangle must be flipped: */
  
  for (loop1=0;loop1<ncount;loop1++)
    {
      dotp=((triangle_normals[a][0]*triangle_normals[tri_neigh[loop1]][0])+
	    (triangle_normals[a][1]*triangle_normals[tri_neigh[loop1]][1])+
	    (triangle_normals[a][2]*triangle_normals[tri_neigh[loop1]][2]));
      
      if (dotp<0)
	{
	
	  /* reverse the normal */
	  if ( culled_tri[tri_neigh[loop1]][4]==0)
	    {
	      old=culled_tri[tri_neigh[loop1]][0];
	      culled_tri[tri_neigh[loop1]][0]=culled_tri[tri_neigh[loop1]][2];
	      culled_tri[tri_neigh[loop1]][2]=old;
	      
	      tri_normal(culled_tri[tri_neigh[loop1]][0],
			 culled_tri[tri_neigh[loop1]][1],
			 culled_tri[tri_neigh[loop1]][2],
			 &triangle_normals[tri_neigh[loop1]][0],
			 &triangle_normals[tri_neigh[loop1]][1],
			 &triangle_normals[tri_neigh[loop1]][2]);
	    }
	}
    }
  
  
  culled_tri[a][5]=1;
  
   for (loop1=0;loop1<ncount;loop1++)
    {
      back_check ( tri_neigh[loop1] );
    }
  
  

  return(0);


}

void povray_out()
{

/* I've primarily written this to interface with the Prepi povray output: Prepi does not output the HOLE surface,
so this function will output some povray commands that you can append to the Prepi file:
*/

  int loop_cntr;

  /* basic texture definitions */
  
  fprintf (stdout,"\n// Povray Mesh file of hole surface: append where neccesary!");
  fprintf (stdout,"\n#declare holefinish = finish {ambient 0.1 diffuse 0.9 phong 0.45 phong_size 10}");
  fprintf (stdout,"\n#declare holetex1 = texture {pigment {color rgbt <1.000,0.000,0.000,0.000>} finish {holefinish}} /* RED           */");
  fprintf (stdout,"\n#declare holetex2 = texture {pigment {color rgbt <0.000,1.000,0.000,0.000>} finish {holefinish}} /* GREEN         */");
  fprintf (stdout,"\n#declare holetex3 = texture {pigment {color rgbt <0.000,0.000,1.000,0.000>} finish {holefinish}} /* BLUE          */");
  fprintf (stdout,"\nmesh {\n");
  

  switch (smooth)
    {
    case 0:
      
      for (loop_cntr=0;loop_cntr<culled_tri_count;loop_cntr++)
	{
	  if (culled_tri[loop_cntr][3]==3)
	    {
	      fprintf (stdout,"\ntriangle { < %f, %f, %f>, < %f, %f,%f>, < %f, %f, %f> texture{holetex1} }",
		       dots[culled_tri[loop_cntr][0]][0],
		       dots[culled_tri[loop_cntr][0]][1],
		       dots[culled_tri[loop_cntr][0]][2],
		       dots[culled_tri[loop_cntr][1]][0],
		       dots[culled_tri[loop_cntr][1]][1],
		       dots[culled_tri[loop_cntr][1]][2],
		       dots[culled_tri[loop_cntr][2]][0],
		       dots[culled_tri[loop_cntr][2]][1],
		       dots[culled_tri[loop_cntr][2]][2]);
	      continue;
	    }
	  
	  if (culled_tri[loop_cntr][3]==7)
	    {
	      fprintf (stdout,"\ntriangle { < %f, %f, %f>, < %f, %f,%f>, < %f, %f, %f> texture{holetex2} }",
		       dots[culled_tri[loop_cntr][0]][0],
		       dots[culled_tri[loop_cntr][0]][1],
		       dots[culled_tri[loop_cntr][0]][2],
		       dots[culled_tri[loop_cntr][1]][0],
		       dots[culled_tri[loop_cntr][1]][1],
		       dots[culled_tri[loop_cntr][1]][2],
		       dots[culled_tri[loop_cntr][2]][0],
		       dots[culled_tri[loop_cntr][2]][1],
		       dots[culled_tri[loop_cntr][2]][2]);
	      continue;
	    }
	  
	  fprintf (stdout,"\ntriangle { < %f, %f, %f>, < %f, %f,%f>, < %f, %f, %f> texture{holetex3} }",
		   dots[culled_tri[loop_cntr][0]][0],
		   dots[culled_tri[loop_cntr][0]][1],
		   dots[culled_tri[loop_cntr][0]][2],
		   dots[culled_tri[loop_cntr][1]][0],
		   dots[culled_tri[loop_cntr][1]][1],
		   dots[culled_tri[loop_cntr][1]][2],
		   dots[culled_tri[loop_cntr][2]][0],
		   dots[culled_tri[loop_cntr][2]][1],
		   dots[culled_tri[loop_cntr][2]][2]);
	}
      break;
      
    case 1:
      vertex_normals();
      /* file format is: smooth_triangle {<corner1>,<normal1>,<corner2>,<normal2>,<corner3>,<normal3> {texture}} */
      
      for (loop_cntr=0;loop_cntr<culled_tri_count;loop_cntr++)
	{
	  if (culled_tri[loop_cntr][3]==3)
	    {
	      fprintf (stdout,"\nsmooth_triangle { < %f, %f, %f>, < %f, %f,%f>,",
		       dots[culled_tri[loop_cntr][0]][0],
		       dots[culled_tri[loop_cntr][0]][1],
		       dots[culled_tri[loop_cntr][0]][2],
		       dots[culled_tri[loop_cntr][0]][4],
		       dots[culled_tri[loop_cntr][0]][5],
		       dots[culled_tri[loop_cntr][0]][6]);

	      fprintf (stdout,"\n < %f, %f, %f>, < %f, %f,%f>,",
		       dots[culled_tri[loop_cntr][1]][0],
		       dots[culled_tri[loop_cntr][1]][1],
		       dots[culled_tri[loop_cntr][1]][2],
		       dots[culled_tri[loop_cntr][1]][4],
		       dots[culled_tri[loop_cntr][1]][5],
		       dots[culled_tri[loop_cntr][1]][6]);
	      
	      fprintf (stdout,"\n < %f, %f, %f>, < %f, %f,%f> texture{holetex1} } ",
		       dots[culled_tri[loop_cntr][2]][0],
		       dots[culled_tri[loop_cntr][2]][1],
		       dots[culled_tri[loop_cntr][2]][2],
		       dots[culled_tri[loop_cntr][2]][4],
		       dots[culled_tri[loop_cntr][2]][5],
		       dots[culled_tri[loop_cntr][2]][6]);
	      
	      continue;
	    }
	  
	  if (culled_tri[loop_cntr][3]==7)
	    {
	      fprintf (stdout,"\nsmooth_triangle { < %f, %f, %f>, < %f, %f,%f>,",
		       dots[culled_tri[loop_cntr][0]][0],
		       dots[culled_tri[loop_cntr][0]][1],
		       dots[culled_tri[loop_cntr][0]][2],
		       dots[culled_tri[loop_cntr][0]][4],
		       dots[culled_tri[loop_cntr][0]][5],
		       dots[culled_tri[loop_cntr][0]][6]);

	      fprintf (stdout,"\n < %f, %f, %f>, < %f, %f,%f>,",
		       dots[culled_tri[loop_cntr][1]][0],
		       dots[culled_tri[loop_cntr][1]][1],
		       dots[culled_tri[loop_cntr][1]][2],
		       dots[culled_tri[loop_cntr][1]][4],
		       dots[culled_tri[loop_cntr][1]][5],
		       dots[culled_tri[loop_cntr][1]][6]);
	      
	      fprintf (stdout,"\n < %f, %f, %f>, < %f, %f,%f> texture{holetex2} } ",
		       dots[culled_tri[loop_cntr][2]][0],
		       dots[culled_tri[loop_cntr][2]][1],
		       dots[culled_tri[loop_cntr][2]][2],
		       dots[culled_tri[loop_cntr][2]][4],
		       dots[culled_tri[loop_cntr][2]][5],
		       dots[culled_tri[loop_cntr][2]][6]);   

   
	      continue;
	    }
	  
	   fprintf (stdout,"\nsmooth_triangle { < %f, %f, %f>, < %f, %f,%f>,",
		    dots[culled_tri[loop_cntr][0]][0],
		    dots[culled_tri[loop_cntr][0]][1],
		    dots[culled_tri[loop_cntr][0]][2],
		    dots[culled_tri[loop_cntr][0]][4],
		    dots[culled_tri[loop_cntr][0]][5],
		    dots[culled_tri[loop_cntr][0]][6]);
	   
	   fprintf (stdout,"\n < %f, %f, %f>, < %f, %f,%f>,",
		    dots[culled_tri[loop_cntr][1]][0],
		    dots[culled_tri[loop_cntr][1]][1],
		    dots[culled_tri[loop_cntr][1]][2],
		    dots[culled_tri[loop_cntr][1]][4],
		    dots[culled_tri[loop_cntr][1]][5],
		    dots[culled_tri[loop_cntr][1]][6]);
	   
	   fprintf (stdout,"\n < %f, %f, %f>, < %f, %f,%f> texture{holetex3} } ",
		    dots[culled_tri[loop_cntr][2]][0],
		    dots[culled_tri[loop_cntr][2]][1],
		    dots[culled_tri[loop_cntr][2]][2],
		    dots[culled_tri[loop_cntr][2]][4],
		    dots[culled_tri[loop_cntr][2]][5],
		    dots[culled_tri[loop_cntr][2]][6]);

	   
	}
      break;
    }
  
  fprintf (stdout,"\n}");
  
  return;
}

/* OSS 1/11/00 vmd_out - based on GC's molscript_out */
void vmd_out ()
{
  
  int loop_cntr=0;
  int current_col=-1000;
        
  /* Print header */
  fprintf (stdout, "draw delete all\n");

  
  /* output the coordinates for each triangle */

 for (loop_cntr=0;loop_cntr<culled_tri_count;loop_cntr++)
   {

     /* is this a different colour? */
     if (culled_tri[loop_cntr][3] != current_col)
        {
	   current_col = culled_tri[loop_cntr][3];
	   switch (current_col)
           {   
	       case(2): { fprintf (stdout,"draw color blue\n");
                          break;
		        }
	       case(3): { fprintf (stdout,"draw color red\n");
                          break;
		        }
               case(7): { fprintf (stdout,"draw color green\n");
                          break;
		        }
	       default: { fprintf (stdout,"draw color yellow\n");
                          break;
		        }
            }	 
	}

     /* are we going for a normal or smoothed surface? */ 
     switch (smooth)
       {  case(0): { fprintf (stdout,"draw triangle ");
                   break;
		   }
          case(1): { fprintf (stdout,"draw trinorm ");
                   break;
		   }
       }		 
  

	
     /* vertex a */
     fprintf (stdout," { %8.3f %8.3f %8.3f } ",      
	       dots[culled_tri[loop_cntr][0]][0],
	       dots[culled_tri[loop_cntr][0]][1],
	       dots[culled_tri[loop_cntr][0]][2]);
     /* vertex b */
     fprintf (stdout," { %8.3f %8.3f %8.3f } ",      
	       dots[culled_tri[loop_cntr][1]][0],
	       dots[culled_tri[loop_cntr][1]][1],
	       dots[culled_tri[loop_cntr][1]][2]);
     /* vertex c */
     fprintf (stdout," { %8.3f %8.3f %8.3f } ",      
	       dots[culled_tri[loop_cntr][2]][0],
	       dots[culled_tri[loop_cntr][2]][1],
	       dots[culled_tri[loop_cntr][2]][2]);	
	       
     	 
     /*do we need normals? */
	 
     if (smooth==1)
       {
         /* normal a */
         fprintf (stdout," { %8.5f %8.5f %8.5f } ",      
	           dots[culled_tri[loop_cntr][0]][4],
	           dots[culled_tri[loop_cntr][0]][5],
	           dots[culled_tri[loop_cntr][0]][6]);
         /* normal b */
         fprintf (stdout," { %8.5f %8.5f %8.5f } ",      
	           dots[culled_tri[loop_cntr][1]][4],
	           dots[culled_tri[loop_cntr][1]][5],
	           dots[culled_tri[loop_cntr][1]][6]);
         /* normal c */
         fprintf (stdout," { %8.5f %8.5f %8.5f } ",      
	           dots[culled_tri[loop_cntr][2]][4],
	           dots[culled_tri[loop_cntr][2]][5],
	           dots[culled_tri[loop_cntr][2]][6]);
        }
	 
     /* Go onto the next line... */
     fprintf (stdout, " \n");
      
   }

 return;
 
}

/* OSS 5/11/00 reorder_triangle */
void reorder_triangle()
{   
  /* 4/11/00 found problem with smoothed surfaces 
     order of outputing triangle critical.  
     From vmd documentation 
     http://www.ks.uiuc.edu/Research/vmd/current/ug/node157.html
      "One caution about defining the vertices and normals: they must be given
       in counter-clockwise order or the shading will be wrong."  
   
      So what we are going to do is 
      (a) add all three normal vectors to get one vector tot_norm
      (b) for triangle
                            A---C
			     \ /
                              B
	find cross product ABcrossAC
      (c) find the dot product = ABcrossAC.tot_norm
      (d) depending on the sign of this dot product
      
          reverse the order of outputing triangles using vbles
          swapping first and second list number
  */
  int iswap;
  double AB[3];
  double AC[3];
  double ABcrossAC[3];
  double tot_norm[3];
  double ABcrossAC_dot_totnom;
  int loop_cntr=0;   
  

 for (loop_cntr=0;loop_cntr<culled_tri_count;loop_cntr++)
   {  /* go thru triangle list */



     
     if (smooth==1)
       { /* only do if producing smooth surface */ 
	       
       /* must check ordering */
       /* (a) add all three normal vectors to get one vector tot_norm */
       tot_norm[0] = dots[culled_tri[loop_cntr][0]][4] +
                     dots[culled_tri[loop_cntr][1]][4] +
                     dots[culled_tri[loop_cntr][2]][4];  /* X */      
       tot_norm[1] = dots[culled_tri[loop_cntr][0]][5] +
                     dots[culled_tri[loop_cntr][1]][5] +
                     dots[culled_tri[loop_cntr][2]][5];  /* Y */    
       tot_norm[2] = dots[culled_tri[loop_cntr][0]][6] +
                     dots[culled_tri[loop_cntr][1]][6] +
                     dots[culled_tri[loop_cntr][2]][6];  /* Z */
       /* (b) for triangle
                            A---C
			     \ /
                              B
	  find cross product ABcrossAC */
          AB[0] = dots[culled_tri[loop_cntr][1]][0] -
	          dots[culled_tri[loop_cntr][0]][0];   /* X */
          AB[1] = dots[culled_tri[loop_cntr][1]][1] -
	          dots[culled_tri[loop_cntr][0]][1];   /* Y */
          AB[2] = dots[culled_tri[loop_cntr][1]][2] -
	          dots[culled_tri[loop_cntr][0]][2];   /* Z */
          
	  AC[0] = dots[culled_tri[loop_cntr][2]][0] -
	          dots[culled_tri[loop_cntr][0]][0];   /* X */
	  AC[1] = dots[culled_tri[loop_cntr][2]][1] -
	          dots[culled_tri[loop_cntr][0]][1];   /* Y */
	  AC[2] = dots[culled_tri[loop_cntr][2]][2] -
	          dots[culled_tri[loop_cntr][0]][2];  /* Z */
	  /* the cross product */ 
	  ABcrossAC[0] = AB[1]*AC[2] - AB[2]*AC[1];
          ABcrossAC[1] = AB[2]*AC[0] - AB[0]*AC[2];
          ABcrossAC[2] = AB[0]*AC[1] - AB[1]*AC[0];
 
	  
          /* (c) find the dot product = ABcrossAC.tot_norm */
          ABcrossAC_dot_totnom = ABcrossAC[0]*tot_norm[0] +
	                         ABcrossAC[1]*tot_norm[1] +
	                         ABcrossAC[2]*tot_norm[2];
				 
          /* (d) depending on the sign of this dot product
             reverse the order of outputing triangles using vbles
             first_write  = 0 or 1
	     second_write = 1 or 0 */
	  if (ABcrossAC_dot_totnom < 0.)
            {
            /* swap culled_tri[loop_cntr][0] with */
	    /*      culled_tri[loop_cntr][1]      */
               iswap = culled_tri[loop_cntr][1];
	       culled_tri[loop_cntr][1] = culled_tri[loop_cntr][0];
	       culled_tri[loop_cntr][0] = iswap;
	    }
		     
       } /* only do if producing smooth surface */ 
   }  /* go thru triangle list */
	  
 return;
 
 
} /* end of: void reorder_triangle() */
void help ()
{
  
  printf("\nsos_triangle: Generates a Hole surface from a sos plot file.");
  printf("\n              (use sph_process to generate the sos file).");
  printf("\n Program reads a sos file from STDIN and writes file to STDOUT.");
  printf("\n \n Usage: surface  [ -h -m -p -r -s -v -l -d ] < infile > outfile");
  printf ("\n OPTIONS:");
  printf ("\n -h Prints this help file.");
  printf ("\n -s Produces a smooth rather than faceted surface.\n");
  printf ("\n -m Produces a Molscript v2.0 surface.\n");
  printf ("\n -d Removes colour information from Molscript objects.\n");
  printf ("\n -l Produces a VRML surface.\n");
  printf ("\n -p Produced as Prepi surface.\n");
  printf ("\n -r Produces a Povray v3.0 surface.\n"); 
  printf ("\n -v Produces a vmd surface (the default).\n"); 
  printf ("\n -X NUMB Use a maximum vertex cull distance of NUMB angs (default 5.0).\n"); 

 return;
}
