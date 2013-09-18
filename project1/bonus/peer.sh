
#cd remote/
echo "start remote client!"
read score
gnome-terminal -e "ssh taochen@lin114-01.cise.ufl.edu cd /cise/homes/taochen/Desktop/remote;sbt run; "
echo "Press enter to continue....."
read score
gnome-terminal -e "ssh taochen@lin114-02.cise.ufl.edu cd /cise/homes/taochen/Desktop/remote;sbt run; "
echo "Press enter to continue..."
read score
gnome-terminal -e "ssh taochen@lin114-03.cise.ufl.edu cd /cise/homes/taochen/Desktop/remote;sbt run; "
echo "Press enter to continue..."
read score
gnome-terminal -e "ssh taochen@lin114-04.cise.ufl.edu cd /cise/homes/taochen/Desktop/remote;sbt run; "
echo "Press enter to continue..."
read score
gnome-terminal -e "ssh taochen@lin114-05.cise.ufl.edu cd /cise/homes/taochen/Desktop/remote;sbt run; "
read score
gnome-terminal -e "ssh taochen@lin114-06.cise.ufl.edu cd /cise/homes/taochen/Desktop/remote;sbt run; "
echo "Press enter to continue....."
read score
gnome-terminal -e "ssh taochen@lin114-07.cise.ufl.edu cd /cise/homes/taochen/Desktop/remote;sbt run; "
echo "Press enter to continue..."
read score
gnome-terminal -e "ssh taochen@lin114-08.cise.ufl.edu cd /cise/homes/taochen/Desktop/remote;sbt run; "
echo "Press enter to continue..."
read score
gnome-terminal -e "ssh taochen@lin114-09.cise.ufl.edu cd /cise/homes/taochen/Desktop/remote;sbt run; "
echo "Press enter to continue..."
read score
gnome-terminal -e "ssh taochen@lin114-10.cise.ufl.edu cd /cise/homes/taochen/Desktop/remote;sbt run; "

echo "start server!"
gnome-terminal -x sh -c "cd /cise/homes/taochen/Desktop/remote;sbt run; "
