#!/usr/bin/env xonsh

from datetime import datetime

def format_timestamp(ts):
    if ts is None:
        return "N/A"
    return datetime.fromtimestamp(ts).strftime("%Y-%m-%d %H:%M:%S")

def format_duration(ts_start, ts_end):
    if ts_start is None or ts_end is None:
        return "N/A"
    duration = ts_end - ts_start
    if duration < 1:
        return f"{duration*1000:.0f}ms"
    elif duration < 60:
        return f"{duration:.2f}s"
    else:
        minutes = int(duration // 60)
        seconds = duration % 60
        return f"{minutes}m {seconds:.1f}s"

def get_xonsh_context():
    context = "### Xonsh Session Information\n\n"
    
    env = __xonsh__.env
    hist = __xonsh__.history
    
    backend_name = env.get('XONSH_HISTORY_BACKEND', 'json')
    data_dir = env.get('XONSH_DATA_DIR', '~/.local/share/xonsh')
    
    context += f"- **Shell**: xonsh\n"
    context += f"- **History Backend**: {backend_name}\n"
    context += f"- **Data Directory**: {data_dir}\n"
    
    if hist and hasattr(hist, 'sessionid'):
        context += f"- **Current Session ID**: {hist.sessionid}\n"
    
    store_stdout = env.get('XONSH_STORE_STDOUT', False)
    context += f"- **Store Output**: {store_stdout}\n"
    
    context += "\n### Command History\n\n"
    
    try:
        commands = []
        
        if hist and hasattr(hist, 'inps') and hist.inps and len(hist.inps) > 0:
            for i in range(len(hist.inps)):
                cmd = {
                    'inp': hist.inps[i],
                    'rtn': hist.rtns[i] if hist.rtns and i < len(hist.rtns) else None,
                    'ts': hist.tss[i] if hist.tss and i < len(hist.tss) else None,
                    'out': hist.outs[i] if hist.outs and i < len(hist.outs) else None,
                    'cwd': hist.cwds[i] if hist.cwds and i < len(hist.cwds) else None,
                }
                commands.append(cmd)
            
            context += f"**Current Session: {len(commands)} commands**\n\n"
        elif backend_name == 'sqlite':
            from xonsh.history.sqlite import _xh_sqlite_get_conn, XH_SQLITE_TABLE_NAME
            
            try:
                with _xh_sqlite_get_conn(filename=hist.filename if hist else None) as conn:
                    cursor = conn.cursor()
                    
                    sql = f"SELECT inp, rtn, tsb, tse, out, cwd FROM {XH_SQLITE_TABLE_NAME} ORDER BY tsb DESC LIMIT 100"
                    cursor.execute(sql)
                    rows = cursor.fetchall()
                    
                    for inp, rtn, tsb, tse, out, cwd in reversed(rows):
                        commands.append({
                            'inp': inp,
                            'rtn': rtn,
                            'ts': [tsb, tse],
                            'out': out,
                            'cwd': cwd,
                        })
                
                context += f"**Recent Commands: {len(commands)} shown**\n\n"
            except Exception as e:
                context += f"- SQLite Fehler: {e}\n"
                return context
        else:
            from xonsh.history.main import construct_history
            hist_backend = construct_history(backend=backend_name, gc=False)
            all_items = list(hist_backend.all_items(newest_first=True))
            
            if all_items:
                commands = []
                for item in all_items[:100]:
                    if isinstance(item, dict):
                        commands.append(item)
                    else:
                        commands.append({
                            'inp': getattr(item, 'cmd', None) or getattr(item, 'inp', None),
                            'rtn': getattr(item, 'rtn', None),
                            'ts': getattr(item, 'ts', None),
                            'out': getattr(item, 'out', None),
                            'cwd': getattr(item, 'cwd', None),
                        })
                
                context += f"**Recent Commands: {len(commands)} shown**\n\n"
        
        if not commands:
            context += "- Keine History-Einträge gefunden\n"
            return context
        
        for idx, cmd in enumerate(commands[-100:], 1):
            inp = cmd.get('inp')
            rtn = cmd.get('rtn')
            ts = cmd.get('ts')
            out = cmd.get('out')
            cwd = cmd.get('cwd')
            
            if inp is None:
                continue
            
            if isinstance(ts, (list, tuple)) and len(ts) >= 2:
                ts_start = ts[0]
                ts_end = ts[1]
            elif isinstance(ts, (int, float)):
                ts_start = ts
                ts_end = None
            else:
                ts_start = None
                ts_end = None
            
            context += f"#### Command {idx}\n"
            context += f"- **Input**: `{inp.strip()}`\n"
            context += f"- **Return Code**: {rtn} {'✓' if rtn == 0 else '✗'}\n"
            context += f"- **Timestamp**: {format_timestamp(ts_start)}\n"
            context += f"- **Duration**: {format_duration(ts_start, ts_end)}\n"
            
            if cwd:
                context += f"- **Working Dir**: `{cwd}`\n"
            
            if out and out.strip():
                out_lines = out.strip().split('\n')
                if len(out_lines) > 5:
                    context += f"- **Output** (first 5 lines):\n```\n"
                    context += '\n'.join(out_lines[:5])
                    context += f"\n... ({len(out_lines)-5} more lines)\n```\n"
                else:
                    context += f"- **Output**:\n```\n{out.strip()}\n```\n"
            
            context += "\n"
        
        return context
        
    except Exception as e:
        context += f"- Fehler beim Laden der History: {e}\n"
        import traceback
        context += f"```\n{traceback.format_exc()}\n```\n"
        return context

print(get_xonsh_context())
