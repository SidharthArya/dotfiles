from datetime import date
import sys
import os
today = date.today()
month = today.strftime("%Y-%m")
filename = "/home/arya/Documents/Org/Bills/Electricity/" + month + ".pdf"
if os.path.exists(filename):
  sys.exit()
  
import json
import time
from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import WebDriverWait

src = "" 
with open('/home/arya/Documents/Org/Bills/variables') as f:
  data = json.load(f)

try:
  with webdriver.Firefox() as driver:
    wait = WebDriverWait(driver, 10)
    driver.get("https://www.bsesdelhi.com/web/brpl/view-duplicate-bill")
    driver.find_element_by_xpath("//input[@maxlength='9']").click()
    for i in data["CA"]:
      driver.find_element_by_xpath("//input[@maxlength='9']").send_keys(i)
      time.sleep(1)
    driver.find_element_by_xpath("//button[@type='submit']").click()
    wait = WebDriverWait(driver, 10)
    driver.find_element_by_xpath("//input[@maxlength='9']").click()
    time.sleep(3)
    for i in data["CA"]:
      driver.find_element_by_xpath("//input[@maxlength='9']").send_keys(i)
      time.sleep(1)
    driver.find_element_by_xpath("//button[@type='submit']").click()
    time.sleep(20)
    src = driver.find_element_by_xpath("//iframe").get_attribute("src")
  import ssl
  ssl._create_default_https_context = ssl._create_unverified_context
  print(src)
  import requests
  response = requests.get(src, verify=False)
  f = open(filename, "bw")
  f.write(response.content)
  f.close()
  import subprocess
  subprocess.run(["zathura", filename]) 

except Exception as e:
  import subprocess
  subprocess.run(["notify-send", "Electricity Bill", str(e)])
