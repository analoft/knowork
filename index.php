<!DOCTYPE html>
<html>
<head>
   <title></title>
   <meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
   </head>
   <body>
   <form method='POST'>
   <h2>PLEASE ENTER SKILL</h2>
 <input type="text" name="skill">
 <input type="submit" value="Test">
 </form>
<?php
//Retrieve name from query string and store to a local variable
$skill = $_POST['skill'];
$command = "Rscript /var/www/html/rscript/knowork/skill_recomm.R $skill";
# to call for multipole skills  pass list as command line argument

//echo $command . "\n";
$out = trim(shell_exec($command));
echo $out

?>
</body>
</html>


