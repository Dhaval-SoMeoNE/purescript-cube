
"use strict"
var speed = 0.0

exports.changeSpeed = function(speed1) {
if (speed1 < 20.0 )
    speed = 0.0;
else if ( speed1 > 1000.0)
    speed = 1000.0;
else 
    speed = speed1;

console.log(speed,"changeSpeed");
return speed1;
};

exports.getSpeed = function(dumy)
{
    console.log(speed,"getSpeed");
return speed;
};
