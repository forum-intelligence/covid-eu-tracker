const puppeteer = require("puppeteer");
var fs = require("fs");

var args = process.argv.slice(2);

(async () => {
  const browser = await puppeteer.launch();
  const page = await browser.newPage();
  await page.goto(`https://coronavirus.data.gov.uk/`);
  await page.waitForSelector(".govuk-table");

  const data = await page.evaluate(() => {
    const tds = Array.from(document.querySelectorAll('table tr td'))
    return tds.map(td => td.innerText)
  });

  fs.writeFile(args[0], JSON.stringify(data), function(err) {
    if (err) throw err;
    console.log("Saved!");
  });

  await browser.close();
})();
