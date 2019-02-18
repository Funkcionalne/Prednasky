const request = require('request');
request('http://dai.fmph.uniba.sk/courses/FPRO/', function (error, response, body) {
  if(error){
    console.log('Error');
  }
  else {
    console.log('Success');
  }
});
--------------------------------------

function callBackHandler(error, response, body) {
  if(error){
    console.log('Error');
    console.log(error);
  }
  else {
    console.log('Success');
  }
}
const request = require('request');
request('http://dai.fmph.unibaa.sk/courses/FPRO/', callBackHandler);

----------------------------------------

const request = require('request');
request('http://dai.fmph.uniba.sk/courses/FPRO/', function (firstError, firstResponse, firstBody) {
    if(firstError){
        console.log('first error');
    }
    else {
        console.log('first success');
        request('http://dai.fmph.uniba.sk/courses/FPRO/{firstBody.path}', function (secondError, secondResponse, secondBody) {
            if(secondError){
              console.log('second error');
            }
            else {
                console.log('second success');
            }
        });
    }
});

-----------------------------------------

Promises
