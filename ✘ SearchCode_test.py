import sqlite3
import os

# 连接到SQLite数据库
conn = sqlite3.connect('yy_OrgCodeNote_my_database.db')
cursor = conn.cursor()

# 遍历数据库中的所有id
cursor.execute("SELECT id, location, find FROM yy_OrgCodeNote_table")  # 请替换your_table_name为你的表名
rows = cursor.fetchall()

for row in rows:
    id, location, find_text = row
    code_num = None

    # 检查find属性是否为纯数字
    if find_text.isdigit():
        code_num = int(find_text)
    else:
        # 检查文件中是否包含find属性的文字
        if os.path.exists(location):
            with open(location, 'r', encoding='utf-8') as file:
                for i, line in enumerate(file, 1):
                    if find_text in line:
                        code_num = i
                        break

    # 更新code_num属性
    if code_num is not None:
        cursor.execute("UPDATE yy_OrgCodeNote_table SET code_num = ? WHERE id = ?", (code_num, id))

# 提交更改并关闭连接
conn.commit()
conn.close()