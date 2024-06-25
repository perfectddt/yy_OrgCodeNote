# -*- coding: utf-8 -*-
import sqlite3
import re

# 创建或连接到数据库
conn = sqlite3.connect('yy_OrgCodeNote_my_database.db')
cursor = conn.cursor()

# 创建表
cursor.execute('''
CREATE TABLE IF NOT EXISTS `yy_OrgCodeNote_table` (
    `id` INTEGER PRIMARY KEY,
    `location` TEXT,
    `find` TEXT,
    `org_num` INTEGER,
    `code_num` INTEGER
)
''')

# 读取文件并处理
def process_file(file_path):
    line_number = 0
    pattern = r'\[\[file:(.*)::(.*)\]\['
    try:
        with open(file_path, 'r', encoding='utf-8') as file:
            for line in file:
                line_number += 1
                match = re.search(pattern, line)
                if match:
                    location = match.group(1)
                    find = match.group(2)
                    cursor.execute('''
                    INSERT INTO yy_OrgCodeNote_table (location, find, org_num)
                    VALUES (?, ?, ?)
                    ''', (location, find, line_number))
        conn.commit()
    except FileNotFoundError:
        print("文件未找到")
    except Exception as e:
        print(f"处理文件时发生错误: {e}")

# 使用示例
file_path = "program_3-代码项目_yy-OrgCodeNote_setup.py.org"  # 替换为您的文件路径
process_file(file_path)

# 关闭数据库连接
conn.close()
