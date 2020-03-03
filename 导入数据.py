import pandas as pd
catering_sale='../data/catering_sale.xls'#读取数据
data=pd.read_excel(catering_sale,index_col=u'日期')#导入数据 
data.describe()#显示基本统计量
