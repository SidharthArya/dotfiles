import json
import time
from robobrowser import RoboBrowser
with open('/home/arya/Documents/Org/Bills/variables') as f:
  data = json.load(f)


browser = RoboBrowser(history=False)
browser.open("https://www.bsesdelhi.com/web/brpl/view-duplicate-bill")

form = browser.get_form()
form['q'].value = 'queen'
browser.submit_form(form)


  
# with webdriver.Chrome() as driver:
#     wait = WebDriverWait(driver, 10)
#     driver.get("https://www.bsesdelhi.com/web/brpl/view-duplicate-bill")
#     driver.find_element_by_xpath("//input[@maxlength='9']").send_keys(data["CA"])
#     driver.find_element_by_xpath("//button[@type='submit']").submit()
#     wait = WebDriverWait(driver, 10)
#     time.sleep(20)
#     url = driver.current_url
#     print(url)
