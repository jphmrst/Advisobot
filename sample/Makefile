ADVISOBOTCP=../advisobot.jar
LIBCP=../../Utils/jmutils.jar

run: Cohort.scala
	mkdir -p reports
	cd ..; make
	scalac -cp ${ADVISOBOTCP}:${LIBCP} -d portfolio.jar Cohort.scala
	scala -cp ${ADVISOBOTCP}:${LIBCP} portfolio.jar
