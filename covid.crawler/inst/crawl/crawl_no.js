const puppeteer = require("puppeteer");
var fs = require("fs");

var args = process.argv.slice(2);

(async () => {
  const browser = await puppeteer.launch();
  const page = await browser.newPage();
  await page.goto(`https://www.fhi.no/sv/smittsomme-sykdommer/corona/dags--og-ukerapporter/dags--og-ukerapporter-om-koronavirus/`);
  await page.waitForSelector("#highcharts-data-table-8");

  const data = await page.evaluate(() => {
    var p = document.getElementsByClassName("highcharts-data-table")[3];
    const tds = Array.from(p.querySelectorAll('table tr td'))
    const ths = Array.from(p.querySelectorAll('table tr th'))
    let v = tds.map(td => td.innerText);
    let n = ths.map(td => td.innerText);
    return [n, v]
  });

  fs.writeFile(args[0], JSON.stringify(data), function(err) {
    if (err) throw err;
    console.log("Saved!");
  });

  await browser.close();
})();
