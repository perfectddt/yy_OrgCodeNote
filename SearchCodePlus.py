
import sqlite3
import os
import argparse

# 创建命令行参数解析器
parser = argparse.ArgumentParser(description='Update database with file contents.')
parser.add_argument('--database', '-d', type=str, required=True, help='The database file path.')
parser.add_argument('--table', '-t', type=str, required=True, help='The table name in the database.')

# 解析命令行参数
args = parser.parse_args()

# 连接到SQLite数据库
conn = sqlite3.connect(args.database)
cursor = conn.cursor()

# 遍历数据库中的所有id
cursor.execute(f"SELECT id, location, find FROM {args.table}")
rows = cursor.fetchall()

# 用于存储已检查的文件路径
checked_locations = set()
# 用于存储文件路径及其内容的映射
file_contents_map = {}

for row in rows:
    id, location, find_text = row
    code_num = None

    # 检查find属性是否为纯数字
    if find_text.isdigit():
        code_num = int(find_text)
    else:
        # 如果文件路径尚未检查，进行检查
        if location not in checked_locations:
            if os.path.exists(location):
                with open(location, 'r', encoding='utf-8') as file:
                    file_contents_map[location] = file.readlines()
            checked_locations.add(location)

        # 检查文件内容中是否包含find属性的文字
        if location in file_contents_map:
            for i, line in enumerate(file_contents_map[location], 1):
                if find_text in line:
                    code_num = i
                    break

    # 更新code_num属性
    if code_num is not None:
        cursor.execute(f"UPDATE {args.table} SET code_num = ? WHERE id = ?", (code_num, id))

# 提交更改并关闭连接
conn.commit()
conn.close()