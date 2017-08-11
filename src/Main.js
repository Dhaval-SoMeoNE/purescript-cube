
"use strict"
var speed = 0.0
var axis = 'x'
var direction = 1.0
var flag = false
exports.changeMouseDown= function(flag1)
{
flag = flag1;
return flag;
}
exports.checkMouseDown= function(flag1)
{
return flag;
}
exports.changeDirection= function(diff)
{
if (diff<0)
    direction = -1.0;
else
    direction = 1.0;
return direction ;
};
exports.getDirection= function(ch)
{
    return direction;
};
exports.changeAxis = function(ch)
{
    axis = ch;
    console.log("Change Axis =", axis);
    return axis;
};
exports.getAxis=function (ch)
{
    console.log("get Axis =", axis);
  return axis;
};
exports.changeSpeed = function(speed1) {
if (speed1 < 20.0 )
    speed = 0.0;
else if ( speed1 > 1500.0)
    speed = 1500.0;
else 
    speed = speed1;

console.log(speed,"changeSpeed");
return speed;
};

exports.getSpeed = function(dumy)
{
    console.log(speed,"getSpeed");
return speed;
};
