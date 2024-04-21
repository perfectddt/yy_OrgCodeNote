import sqlite3

def merge_databases(main_db_path, other_db_path):
    # 连接到主数据库
    conn_main = sqlite3.connect(main_db_path)
    conn_other = sqlite3.connect(other_db_path)

    # 获取其他数据库的表名
    cursor_other = conn_other.cursor()
    cursor_other.execute("SELECT name FROM sqlite_master WHERE type='table';")
    tables_other = cursor_other.fetchall()

    # 合并表
    for table_name, in tables_other:
        cursor_other.execute(f"SELECT * FROM '{table_name}';")
        data = cursor_other.fetchall()
        cursor_main = conn_main.cursor()

        # 检查主数据库中是否存在表
        cursor_main.execute(f"SELECT COUNT(*) FROM sqlite_master WHERE type='table' AND name=?", (table_name,))
        if cursor_main.fetchone()[0] == 1:
            # 如果表存在，直接插入数据
            for row in data:
                try:
                    cursor_main.execute(f"INSERT INTO '{table_name}' VALUES ({('?,' * len(row))[:-1]});", row)
                except sqlite3.IntegrityError:
                    # 处理唯一约束冲突
                    print(f"Unique constraint violation for table {table_name}. Skipping row.")
            conn_main.commit()
        else:
            # 如果表不存在，创建表并插入数据
            cursor_other.execute(f"SELECT sql FROM sqlite_master WHERE type='table' AND name=?", (table_name,))
            create_table_sql = cursor_other.fetchone()[0]
            cursor_main.execute(create_table_sql)
            cursor_main.executemany(f"INSERT INTO '{table_name}' VALUES ({('?,' * len(data[0]))[:-1]});", data)
            conn_main.commit()

    # 提交更改并关闭连接
    conn_other.close()
    conn_main.close()

if __name__ == "__main__":
    import argparse
    # 设置命令行参数解析
    parser = argparse.ArgumentParser(description='Merge SQLite databases.')
    parser.add_argument('--main', type=str, required=True, help='Path to the main SQLite database.')
    parser.add_argument('--other', type=str, required=True, help='Path to the other SQLite database to merge.')
    
    args = parser.parse_args()

    # 执行合并操作
    merge_databases(args.main, args.other)