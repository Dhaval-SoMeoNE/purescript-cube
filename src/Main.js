
"use strict"
var speed = 0.0
var flag1 = false


exports.changeSpeed = function(speed1) {
if (speed1 < 20.0 )
    speed = 0.0;
else if ( speed1 > 400.0)
    speed = 200.0;
else 
    speed = speed1;

console.log(speed,"changeSpeed");
return speed1;
};

exports.flag = function (temp)
{
    flag1 = ! flag1;
    return flag1;
}
exports.getSpeed = function(dumy)
{
    console.log(speed,"getSpeed");
return speed;
};