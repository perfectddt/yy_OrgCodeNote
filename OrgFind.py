# -*- coding: utf-8 -*-
import sqlite3
import re
import os
import argparse

# 创建命令行参数解析器
parser = argparse.ArgumentParser(description='Process a file and specify a database and table name.')
parser.add_argument('--file', '-f', type=str, required=True, help='The file path to process.')
parser.add_argument('--database', '-d', type=str, required=True, help='The database file path.')
parser.add_argument('--table', '-t', type=str, required=True, help='The table name in the database.')

# 解析命令行参数
args = parser.parse_args()

# 创建或连接到数据库
conn = sqlite3.connect(args.database)
cursor = conn.cursor()

# 创建表
create_table_query = f'''
CREATE TABLE IF NOT EXISTS `{args.table}` (
    `id` INTEGER PRIMARY KEY,
    `location` TEXT,
    `org_location` TEXT,
    `find` TEXT,
    `org_num` INTEGER,
    `code_num` INTEGER
)
'''
cursor.execute(create_table_query)

# 读取文件并处理
def process_file(file_path, table_name):
    line_number = 0
    pattern = r'\[\[file:(.*)::(.*)\]\['
    try:
        with open(file_path, 'r', encoding='utf-8') as file:
            for line in file:
                line_number += 1
                match = re.search(pattern, line)
                if match:
                    # 将相对位置转换为绝对位置
                    location = os.path.abspath(os.path.join(os.path.dirname(file_path), match.group(1)))
                    find = match.group(2)
                    cursor.execute(f'''
                    INSERT INTO `{table_name}` (location, find, org_num,org_location)
                    VALUES (?, ?, ?, ?)
                    ''', (location, find, line_number,file_path))
        conn.commit()
    except FileNotFoundError:
        print("文件未找到")
    except Exception as e:
        print(f"处理文件时发生错误: {e}")

# 使用命令行参数调用函数
process_file(args.file, args.table)

# 关闭数据库连接
conn.close()