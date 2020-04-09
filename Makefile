# Assumes GPR_PROJECT_PATH includes Anet installation
# Try to use the same GNAT to build adatftpd that was used to
# build Anet.
all:
	gprbuild -p -P adatftpd.gpr

analyse:
	codepeer -P adatftpd.gpr --no-subprojects -file adatftpd.adb -quiet -f -j0 -level 4

review: analyse
	codepeer -P adatftpd.gpr -quiet -file adatftpd.adb -compiler-mode -output-msg --complete-output
	codepeer -P adatftpd.gpr -quiet -file adatftpd.adb --gnatcheck -output-msg

clean:
	gprclean -q -P adatftpd.gpr
