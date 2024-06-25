# -*- coding: utf-8 -*-
import sqlite3

# 连接到数据库
conn = sqlite3.connect('yy_OrgCodeNote_my_database.db')
cursor = conn.cursor()

# 查询数据库中的所有记录
cursor.execute('SELECT * FROM yy_OrgCodeNote_table')

# 获取所有记录列表
rows = cursor.fetchall()

# 打印查询结果
for row in rows:
    print(row)

# 关闭数据库连接
conn.close()