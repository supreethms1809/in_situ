NETCDF ?= /usr
PNETCDF ?= /usr
PIO ?= /home/mpas/Desktop/Source/pio/trunk/pio
GEN_F90 ?= true
PARAVIEW ?= /home/mpas/Desktop/Source/PV/build
USE_PARAVIEW ?= true
CORE ?= ocean
#DEBUG ?= true

dlft:
	( $(MAKE) gfortran )
