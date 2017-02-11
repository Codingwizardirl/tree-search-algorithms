#
# Propositional Inference
# Informatics 2D
# Assignment 1
# 2016-2017
#
# Makefile
#

OBJS 	= Prop.o Inf2d.o Main/SatParser.o 
INTERS  = Prop.hi Inf2d.hi Main/SatParser.hi 
SOURCE	= Prop.hs Inf2d.hs Main/SatParser.hs 
CC	= ghc
FLAGS   = -package parsec -package random -iMain -c
OFLAGS  = -package parsec -package random -iMain

%.o: %.hs
	$(CC) $(FLAGS) $<

%.hi: %.hs
	$(CC) $(FLAGS) $<

walksat: $(OBJS) $(INTERS) Main/WalkSat.o Main/WalkSat.hi
	$(CC) $(OFLAGS) $(OBJS) Main/WalkSat.o -o walksat

dpll: $(OBJS) $(INTERS) Main/Dpll.o Main/Dpll.hi
	$(CC) $(OFLAGS) $(OBJS) Main/Dpll.o -o dpll

dpllv2: $(OBJS) $(INTERS) Main/Dpllv2.o Main/Dpllv2.hi
	$(CC) $(OFLAGS) $(OBJS) Main/Dpllv2.o -o dpllv2

all: walksat dpll dpllv2

clean:
	rm -f $(OBJS) $(INTERS) Main/WalkSat.o Main/WalkSat.hi Main/Dpll.o Main/Dpll.hi Main/Dpllv2.o Main/Dpllv2.hi

cleanall: clean
	rm -f walksat dpll dpllv2

count:
	wc $(SOURCE)

