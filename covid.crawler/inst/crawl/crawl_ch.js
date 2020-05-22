const puppeteer = require("puppeteer");
var fs = require("fs");

var args = process.argv.slice(2);

(async () => {
  const browser = await puppeteer.launch();
  const page = await browser.newPage();
  await page.goto(`https://interactif.tdg.ch/2020/covid-19-carte-suisse/`);
  await page.waitForSelector(".swissCasesTable");

  var cases = await page.evaluate(() => {
    var tables = document.querySelectorAll(`.swissCasesTable`);
    var table = [];
    
    for(var t = 0; t < tables.length; t++){
      const tds = Array.from(tables[t].querySelectorAll('table tr'));
      var rows =  tds.map(td => td.innerHTML);
      table.push(rows);
    }

    return table;
  });

  fs.writeFile(args[0], JSON.stringify(cases), function(err) {
    if (err) throw err;
    console.log("Saved!");
  });

  await browser.close();
})();
