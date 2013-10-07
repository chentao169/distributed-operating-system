#!/bin/sh
# put this script file and project.scala in a same directory and run it
# I am not sure this script would work. if not, google it
# you can add more test on pushsum, I did not test it

scalac project2
echo "compilation ends"
cd ./target

echo "scala project2 10000 full gossip"
for run in {1..10}
do
	scala project2 10000 full gossip > result.txt
	echo $run
done

echo "scala project2 100 line gossip"
for run in {1..10}
do
	scala project2 100 line gossip >> result.txt
	echo $run
done

echo "scala project2 1000 grid gossip"
for run in {1..10}
do
	scala project2 1000 grid gossip >> result.txt
	echo $run
done

echo "scala project2 10000 imperfectgrid gossip"
for run in {1..10}
do
	scala project2 10000 imperfectgrid gossip >> result.txt
	echo $run
done

exit 0

