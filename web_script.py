from requests_html import HTMLSession
session = HTMLSession()
r = session.get('http://sousuo.gov.cn/s.htm?t=zhengcelibrary&q=%E7%A4%BE%E4%BC%9A%E7%BB%84%E7%BB%87&timetype=timeqb&mintime=&maxtime=&sort=&sortType=1&searchfield=&pcodeJiguan=&bmfl=&childtype=&subchildtype=&tsbq=&pubtimeyear=&puborg=&pcodeYear=&pcodeNum=&filetype=&p=0&n=5&orpro=&inpro=')
div_list=r.html.find('p.dysMiddleResultConItemMemo')
for div in div_list:
    print(div.text)