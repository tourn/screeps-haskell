var https = require('https');
var fs = require('fs');
var args = process.argv.slice(2);

var email = args[0],
    password = args[1],
    data = {
        branch: 'haskell',         
        modules: {
            main: fs.readFileSync('.stack-work/dist/x86_64-linux/Cabal-1.24.2.0_ghcjs/build/screeps-exe/screeps-exe.jsexe/all.js', 'utf8')
        }
    };

var req = https.request({
    hostname: 'screeps.com',
    port: 443,
    path: '/api/user/code',
    method: 'POST',
    auth: email + ':' + password,
    headers: {
        'Content-Type': 'application/json; charset=utf-8'
    }}, function(res){
      console.log("!");
      res.setEncoding('utf8');
      res.on('data', function(chunk){
        console.log(chunk);
      });
      res.on('end', function(){
      });
    }
);

req.on('error', function(e){
	console.log(e);
});

req.write(JSON.stringify(data));
req.end();
