# Assumes GPR_PROJECT_PATH includes Anet installation
# Try to use the same GNAT to build adatftpd that was used to 
# build Anet.
all:
	gprbuild -P adatftpd.gpr
	
clean:
	gprclean -q -P adatftpd.gpr
