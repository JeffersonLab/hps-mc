FF = g77
FFLAGS = -O -i4 -ffixed-line-length-132 
#FFLAGS = -O -i4 -fno-automatic -ffixed-line-length-132 
#FFLAGS = -g -C -i4
OBJECTS = driver.o particles.o sort.o readproc.o tops.o fields.o create.o\
colors.o tsums.o fsums.o ftsums.o writeamp.o gencolor.o eigen_lapack.o\
lapack.o fermi4.o drawfeyn.o post.o permute.o vert3.o vert4.o \
qed.o qcd.o qfd.o readmodel.o nvert3.o nvert4.o writedsig.o crossing.o \
write_configs.o leshouche.o

OBJECTS1= particles.o sort.o readproc.o tops.o fields.o create.o\
colors.o tsums.o fsums.o ftsums.o writeamp.o gencolor.o eigen_lapack.o\
lapack.o fermi4.o drawfeyn.o post.o permute.o vert3.o vert4.o \
qed.o qcd.o qfd.o readmodel.o nvert3.o nvert4.o writedsig.o crossing.o \
write_configs.o leshouche.o dipolcolor.o maindipol.o

.f.o:; $(FF) $(FFLAGS) -c $*.f

all: mg2 mg2_sa mg2_mw mg2_dip

mg2:$(OBJECTS) makefile
	$(FF) $(FFLAGS) -o mg2 $(OBJECTS)

mgtest:$(OBJECTS) makefile
	$(FF) $(FFLAGS) -o mgtest $(OBJECTS)

gen_proc_id:gen_proc_id.o particles.o  utility.o
	$(FF) $(FFLAGS) -o gen_proc_id gen_proc_id.o particles.o utility.o 

mg2_sa:$(OBJECTS) makefile
	sed -i.bak -e 's/StandAlone\/\.false\.\//StandAlone\/\.true\.\//g' writeamp.f
	$(FF) $(FFLAGS) -c writeamp.f
	$(FF) $(FFLAGS) -o mg2_sa $(OBJECTS)
	sed -i.bak -e 's/StandAlone\/\.true\.\//StandAlone\/\.false\.\//g' writeamp.f
	$(FF) $(FFLAGS) -c writeamp.f

mg2_mw:$(OBJECTS) makefile
	sed -i.bak -e 's/MadWeight\/\.false\.\//MadWeight\/\.true\.\//g' driver.f
	$(FF) $(FFLAGS) -c driver.f
	$(FF) $(FFLAGS) -o mg2_mw $(OBJECTS)
	sed -i.bak -e 's/MadWeight\/\.true\.\//MadWeight\/\.false\.\//g' driver.f
	$(FF) $(FFLAGS) -c driver.f

mg2_dip:$(OBJECTS1) makefile
	sed -i.bak -e 's/DipolSub\/\.false\.\//DipolSub\/\.true\.\//g' particles.f
	sed -i.bak -e 's/DipolSub\/\.false\.\//DipolSub\/\.true\.\//g' writeamp.f
	sed -i.bak -e 's/DipolSub\/\.false\.\//DipolSub\/\.true\.\//g' writedsig.f
	$(FF) $(FFLAGS) -c particles.f
	$(FF) $(FFLAGS) -c writeamp.f
	$(FF) $(FFLAGS) -c writedsig.f
	$(FF) $(FFLAGS) -o mg2_dip $(OBJECTS1)
	sed -i.bak -e 's/DipolSub\/\.true\.\//DipolSub\/\.false\.\//g' particles.f
	sed -i.bak -e 's/DipolSub\/\.true\.\//DipolSub\/\.false\.\//g' writeamp.f
	sed -i.bak -e 's/DipolSub\/\.true\.\//DipolSub\/\.false\.\//g' writedsig.f
	$(FF) $(FFLAGS) -c particles.f
	$(FF) $(FFLAGS) -c writeamp.f
	$(FF) $(FFLAGS) -c writedsig.f

colors.o : colors.f params.inc
create.o : create.f params.inc
crossing.o : crossing.f params.inc
drawfeyn.o : drawfeyn.f params.inc
driver.o : driver.f params.inc
dsig.o : dsig.f params.inc
eigen_lapack.o : eigen_lapack.f params.inc
fermi4.o : fermi4.f params.inc
fields.o : fields.f params.inc
fsums.o : fsums.f params.inc
ftsums.o : ftsums.f params.inc
gencolor.o : gencolor.f params.inc
genpp.o : genpp.f params.inc
lapack.o : lapack.f
nvert3.o : nvert3.f params.inc
nvert4.o : nvert4.f params.inc
opt.o : opt.f params.inc
particles.o : particles.f params.inc
permute.o : permute.f params.inc
post.o : post.f params.inc
qcd.o : qcd.f params.inc
qed.o : qed.f params.inc
qfd.o : qfd.f params.inc
readmodel.o : readmodel.f params.inc
readproc.o : readproc.f params.inc
sort.o : sort.f params.inc
tops.o : tops.f params.inc
tsums.o : tsums.f params.inc
vert3.o : vert3.f params.inc
vert4.o : vert4.f params.inc
writedsig.o : writedsig.f params.inc
writesub.o : writesub.f params.inc
writeamp.o : writeamp.f params.inc
write_configs.o: write_configs.f params.inc
dipolcolor.o: dipolcolor.f params.inc
