let program = require('commander');
let YAML = require('yamljs');
let R = require('ramda');
let Handlebars = require('handlebars');
let fs = require('fs');
let mapIndexed = R.addIndex( R.map );
let opTpl = fs.readFileSync(__dirname + "/operation-type.handlebars", "utf-8")
let path = require('path');


Handlebars.registerHelper('lowerFirst', str =>
  str.length > 0
    ? str[0].toLowerCase() + str.substr(1)
    : str
);



generateOperationKindData("src/SEd/Operations/OperationKindData.yaml")


program
  .version('0.0.1')
  .usage('[options] <file ...>')
  .parse(process.argv);


program.args.forEach( file => generateOperationKindData( file ))

function getOutputName(inFile) {
  return path.join(
    path.dirname(inFile),
    path.basename(inFile, '.yaml') + '.elm'
  );
}

function generateOperationKindData(inFile) {

  let tpl = Handlebars.compile( opTpl );

  YAML.load(inFile, (result)=> {
    let outPath = getOutputName(inFile);
    let res = tpl(result);

    fs.writeFile(outPath, res, "utf-8", (err)=>{
      if (err) { return console.error(err); }
      console.log('+ %s', outPath)
    });
    // console.log(tpl(result))

  });
}




