import { chromium } from "playwright";

const url = process.argv[2];

(async () => {
  const browser = await chromium.launch({
    headless: false, // 👈 mostrar navegador
  });

  // Usar perfil de usuario real
  const context = await browser.newContext({
    viewport: { width: 1280, height: 800 },
    userAgent: "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/117.0.0.0 Safari/537.36"
  });

  const page = await context.newPage();
  await page.goto(url, { waitUntil: "networkidle", timeout: 90000 });

  // Simular scroll para que se cargue el menú
  await page.evaluate(() => {
    window.scrollBy(0, window.innerHeight);
  });
  await page.waitForTimeout(3000);

  // Obtener HTML completo
  const html = await page.content();
  console.log(html);

  await browser.close();
})();

