c**************************************************************************
c
c	Parameters used by MadGraphII in many different subroutines
c
c**************************************************************************
      double precision zero    , one    
      parameter       (zero=0d0, one=1d0)
c**************************************************************************
c
c     Careful if you want to change the maximum number of external particles
c     you also have to change several other parameters:
c           maxtops is probably the most important. Also look at some
c           of the other QCD color related parameters.
c
c**************************************************************************
      integer    maxlines  
      parameter (maxlines=11)  !This is the number of external lines+1
c**************************************************************************
c
c     for nexteral = 7, 8, 9, 10  -> maxtops = 2485, 34300, 559405, 10525900
c
c**************************************************************************
      integer    maxtops       ,maxnodes           ,maxgraphs
      parameter (maxtops=7000   ,maxnodes=maxlines-3,maxgraphs=7000 )
      integer    max_fermi
      parameter (max_fermi=720)  !Used in fermi4. Should be (maxlines/2)!
      integer    max_draw
      parameter (max_draw=maxgraphs)  !Maximum number of diagrams to draw postscript

c**************************************************************************
c
c     Below are some QCD color parameters
c
c**************************************************************************
      integer    Nc
      parameter (Nc=3)        !Number of colors SU(3)
      integer    maxfactors
      parameter (maxfactors=15)!Max number of color factors T(a,b,c)T(c,d,e)...
      integer    maxterms
      parameter (maxterms=256)!Max number of terms from expanding color
      integer    maxflows
      parameter (maxflows=3000 )!Max number of color flows (string configs)
      integer    maxcolors
      parameter (maxcolors=3000 )!Max size of reduced color matrix
c**************************************************************************
c
c     Below are some General size parameters
c
c**************************************************************************
      integer    max_coup     
      parameter (max_coup=5)  !Max number of unique couplings QCD/QED/QFD ...
      integer    max_particles  !Need to change V3 and V4 if change here
      parameter (max_particles=2**7-1) !Max num of particles+antiparticle
      integer    isubfile
      parameter (isubfile=28)  !Unit number for file
      integer    itnum
      parameter (itnum=23)    !Unit number for temporary file can be used again
      integer    max_string
      parameter (max_string=120) !Largest line of ouput call iovxxx(.......)
c**************************************************************************
c
c     Below are parameters related to crossings, and pp->Wjjj 
c
c**************************************************************************
      integer    maxpartons      !Used for summing
      parameter (maxpartons=20)  !Number of partons in proton u,d,s,c,b,t,g,...
      integer    maxloops      !Used for crossings
      parameter (maxloops=10)  !Number multiparticle P,J,l+,l- etc
      integer    npartons      !Used for crossings
      parameter (npartons=9)  !Number of partons in proton u,d,s,c,b,t,g,...
      integer    nparts      !Used for crossings
      parameter (nparts=9)  !Number of partons in proton u,d,s,c,b,t,g,...
      integer    maxcross
      parameter (maxcross=3)!Max number of crossings for a process
      integer    maxrows  ,maxcols
c      parameter (maxrows=6,maxcols=720) !Permutations in genpp,drawfeyn
      parameter (maxrows=7,maxcols=5040) !Permutations in genpp,drawfeyn
      integer    maxproc
      parameter (maxproc=20)     !Largest number of subprocess for pp->jj
                                !Careful these are all in memory for optimiz.
c**************************************************************************
c
c     Postscript Feynman diagram parameters
c
c**************************************************************************
      integer    graphs_per_row  , scale   , rows_per_page
      parameter (graphs_per_row=2, scale=10, rows_per_page=4) !For Diagrams
      integer    graphs_per_page
      parameter (graphs_per_page=rows_per_page*graphs_per_row)
c**************************************************************************
c
c     Parameters for reading in subprocesses/Models
c
c**************************************************************************
      integer    fermion  
      parameter (fermion = 1)   !Used to identify fermions for fermi crossing 
      integer    max_words   , max_chars   , maxline    !For reading in model
      parameter (max_words=20, max_chars=70, maxline=80)
      character*1 null
      parameter  (null=' ')
      integer   maxv3     , maxv4     
      parameter(maxv3=11000, maxv4=4000) !Maximum number of 3/4-vertices in sort
      integer    max_blocks            !Maximim number of blocks of couplings
      parameter (max_blocks=5)         !which may be selectively written out
c**************************************************************************
c
c     Parameters for the dipole generation
c
c**************************************************************************
      integer    maxdipoles
      parameter (maxdipoles=200)
