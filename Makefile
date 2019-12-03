# Assumes GPR_PROJECT_PATH includes Anet installation
# Try to use the same GNAT to build adatftpd that was used to 
# build Anet.
all:
	gprbuild -p -P adatftpd.gpr
	
check:
	gnatcheck -P adatftpd.gpr --show-rule -rules -from=gnatcheck.rules
	
clean:
	gprclean -q -P adatftpd.gpr
