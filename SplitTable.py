import sqlite3
import os
import shutil
import argparse
import MergeDB
import re
def split_table_into_multiple_dbs(database_path, table_name):
    # 确保数据库文件存在
    if not os.path.exists(database_path):
        print(f"Error: Database file '{database_path}' does not exist.")
        return

    # 连接到原始数据库
    conn = sqlite3.connect(database_path)
    cursor = conn.cursor()

    # 使用方括号包裹表名以避免特殊字符问题
    wrapped_table_name = f"{table_name}"
    # wrapped_table_name = f'{table_name}'
    # 检查原始表是否存在
    cursor.execute(f"SELECT name FROM sqlite_master WHERE type='table' AND name=?", (wrapped_table_name,))
    if cursor.fetchone() is None:
        print(f"Error: Table '{table_name}' does not exist in the database.")
        return

    # 查询所有不同的location值
    print(f"SELECT DISTINCT location FROM \"{wrapped_table_name}\"")
    cursor.execute(f"SELECT DISTINCT location FROM \"{wrapped_table_name}\"")
    locations = cursor.fetchall()

    # 对每个location进行处理
    for location in locations:
        location = location[0]  # 获取元组中的location值
        file_name = os.path.basename(location)# 处理file_name，替换不合规则的字符
        file_name = re.sub(r'[\$+\-\. ]', '_', file_name)  # 使用正则表达式替换
        new_table_name = f"{file_name}_table"
        
        # 检查新表是否已存在
        cursor.execute(f"SELECT name FROM sqlite_master WHERE type='table' AND name=?", (new_table_name,))
        if cursor.fetchone():
            # 如果表已存在，删除它
            cursor.execute(f"DROP TABLE IF EXISTS '{new_table_name}'")
            conn.commit()

        # 在原始数据库中创建新表
        cursor.execute(f"CREATE TABLE '{new_table_name}' AS SELECT * FROM '{wrapped_table_name}' WHERE location=?", (location,))
        conn.commit()

        # 在location对应的文件夹路径下创建新数据库文件
        new_db_path = os.path.join(os.path.dirname(location), 'yy_orgcodenote_my_database.db')
        print(new_db_path)
        if not os.path.exists(os.path.dirname(new_db_path)):
            os.makedirs(os.path.dirname(new_db_path))
        # 创建新数据库文件
        with open(new_db_path, 'wb') as f:
            pass  # 创建空文件
        # 移动数据库文件
        # shutil.copy(database_path, new_db_path)
        MergeDB.merge_databases(new_db_path,database_path)

        # 连接到新数据库
        new_conn = sqlite3.connect(new_db_path)
        new_cursor = new_conn.cursor()



        # 提交更改并关闭新数据库连接
        new_conn.commit()
        new_conn.close()

    # 关闭原始数据库连接
    conn.close()

if __name__ == "__main__":
    # 设置命令行参数解析
    parser = argparse.ArgumentParser(description='Split a table in a SQLite database into multiple databases based on location.')
    parser.add_argument('--database', '-d', type=str, required=True, help='The path to the SQLite database.')
    parser.add_argument('--table', '-t', type=str, required=True, help='The table name in the database.')
    
    # 解析命令行参数
    args = parser.parse_args()

    # 调用函数，传入数据库路径和原始表名
    split_table_into_multiple_dbs(args.database, args.table)