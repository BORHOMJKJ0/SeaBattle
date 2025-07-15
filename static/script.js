console.log('script.js loaded');

const SERVER_URL = 'http://localhost:8080';
const API_ENDPOINTS = {
    validate: `${SERVER_URL}/api/validate`,
    grid: `${SERVER_URL}/api/grid`,
    status: `${SERVER_URL}/api/status`
};

let currentGrid = {
    size: 0,
    cells: [],
    rowRequirements: [],
    colRequirements: [],
    ships: []
};

async function handleServerResponse(response) {
    const contentType = response.headers.get('content-type');
    
    if (!response.ok) {
        let errorMessage = `HTTP ${response.status}: ${response.statusText}`;
        
        try {
            if (contentType && contentType.includes('application/json')) {
                const errorData = await response.json();
                if (errorData.message) {
                    errorMessage += ` - ${errorData.message}`;
                }
            } else {
                const errorText = await response.text();
                if (errorText) {
                    errorMessage += ` - ${errorText.substring(0, 200)}`;
                }
            }
        } catch (e) {
            console.warn('Could not parse error response:', e);
        }
        
        throw new Error(errorMessage);
    }
    
    if (!contentType || !contentType.includes('application/json')) {
        const text = await response.text();
        console.error('Expected JSON but got:', contentType, 'Content:', text.substring(0, 200));
        throw new Error('الخادم لم يرجع JSON صحيح. نوع المحتوى: ' + (contentType || 'غير محدد'));
    }
    
    const buffer = await response.arrayBuffer();
    const decoder = new TextDecoder('utf-8');
    const text = decoder.decode(buffer);
    console.log('Raw response decoded:', text);
    
    try {
        return JSON.parse(text);
    } catch (parseError) {
        console.error('JSON parsing error:', parseError);
        console.error('Response text:', text);
        throw new Error('خطأ في تحليل الاستجابة من الخادم: ' + parseError.message);
    }
}

async function checkServerStatus() {
    const statusElement = document.getElementById('server-status');
    try {
        const res = await fetch(API_ENDPOINTS.status, {
            method: 'GET',
            headers: {
                'Content-Type': 'application/json; charset=UTF-8',
                'Accept': 'application/json; charset=UTF-8'
            }
        });
        
        const data = await handleServerResponse(res);
        
        if (data.status === 'online') {
            statusElement.className = 'server-status online';
            statusElement.textContent = '🟢 متصل بالخادم';
            logToConsole('✅ تم الاتصال بالخادم بنجاح');
            return true;
        } else {
            throw new Error('Server not ready');
        }
    } catch (e) {
        statusElement.className = 'server-status offline';
        statusElement.textContent = '🔴 غير متصل بالخادم';
        logToConsole('❌ فشل الاتصال بالخادم: ' + e.message);
        return false;
    }
}

async function loadGridFromServer() {
    const loadBtn = document.getElementById('load-btn');
    loadBtn.disabled = true;
    clearResults();
    
    try {
        logToConsole('📥 جاري تحميل البيانات من الخادم...');
        
        const res = await fetch(API_ENDPOINTS.grid, {
            method: 'GET',
            headers: {
                'Content-Type': 'application/json; charset=UTF-8',
                'Accept': 'application/json; charset=UTF-8'
            }
        });
        
        const data = await handleServerResponse(res);
        console.log('Server response:', data);

        if (data.size && data.grid && data.row_requirements && data.col_requirements) {
            currentGrid.size = data.size;
            currentGrid.cells = [];
            currentGrid.rowRequirements = data.row_requirements.map(r => r.required);
            currentGrid.colRequirements = data.col_requirements.map(c => c.required);
            currentGrid.ships = data.ships || [];

            for (let r = 0; r < currentGrid.size; r++) {
                currentGrid.cells[r] = [];
                for (let c = 0; c < currentGrid.size; c++) {
                    const cellData = data.grid[r][c];
                    currentGrid.cells[r][c] = cellData.type === 'ship' ? 'ship' : 'sea';
                }
            }

            renderGrid();
            updateColumnRequirements();
            logToConsole('✅ تم تحميل البيانات بنجاح');
            logToConsole(`📊 حجم الشبكة: ${currentGrid.size}x${currentGrid.size}`);
            logToConsole(`📊 متطلبات الصفوف: [${currentGrid.rowRequirements.join(', ')}]`);
            logToConsole(`📊 متطلبات الأعمدة: [${currentGrid.colRequirements.join(', ')}]`);
        } else {
            throw new Error('Invalid data format from server');
        }
    } catch (e) {
        logToConsole('❌ خطأ في تحميل البيانات: ' + e.message);
        alert('خطأ في تحميل البيانات من الخادم: ' + e.message);
    } finally {
        loadBtn.disabled = false;
    }
}

function updateColumnRequirements() {
    const container = document.getElementById('requirements-top');
    container.innerHTML = '';

    const emptyCell = document.createElement('div');
    emptyCell.className = 'req-cell-empty';
    container.appendChild(emptyCell);

    for (let i = 0; i < currentGrid.size; i++) {
        const div = document.createElement('div');
        div.className = 'req-cell';
        div.textContent = currentGrid.colRequirements[i] || '0';
        container.appendChild(div);
    }

    container.style.gridTemplateColumns = `60px repeat(${currentGrid.size}, 60px)`;
}

function renderGrid() {
    const boardRows = document.getElementById('board-rows');
    boardRows.innerHTML = '';

    for (let r = 0; r < currentGrid.size; r++) {
        const rowDiv = document.createElement('div');
        rowDiv.className = 'board-row';

        const rowReqDiv = document.createElement('div');
        rowReqDiv.className = 'row-req';
        rowReqDiv.textContent = currentGrid.rowRequirements[r] || '0';
        rowDiv.appendChild(rowReqDiv);

        const gridDiv = document.createElement('div');
        gridDiv.className = 'grid';
        gridDiv.style.gridTemplateColumns = `repeat(${currentGrid.size}, 60px)`;

        for (let c = 0; c < currentGrid.size; c++) {
            const cell = document.createElement('div');
            cell.className = 'cell';
            cell.dataset.row = r;
            cell.dataset.col = c;
            
            const cellType = currentGrid.cells[r][c];
            cell.textContent = cellType === 'ship' ? '🚢' : '🌊';
            cell.classList.add(cellType === 'ship' ? 'ship' : 'sea');

            gridDiv.appendChild(cell);
        }

        rowDiv.appendChild(gridDiv);
        boardRows.appendChild(rowDiv);
    }
}

function toggleCell(row, col) {
    const currentType = currentGrid.cells[row][col];
    const newType = currentType === 'ship' ? 'sea' : 'ship';
    
    currentGrid.cells[row][col] = newType;
    
    const cell = document.querySelector(`[data-row="${row}"][data-col="${col}"]`);
    if (cell) {
        cell.textContent = newType === 'ship' ? '🚢' : '🌊';
        cell.className = 'cell ' + newType;
    }
    
    clearResults();
    logToConsole(`🔄 تم تغيير الخلية (${row + 1}, ${col + 1}) إلى ${newType === 'ship' ? 'سفينة' : 'بحر'}`);
}

function resetGrid() {
    if (currentGrid.size === 0) {
        alert('يرجى تحميل الشبكة أولاً');
        return;
    }

    for (let r = 0; r < currentGrid.size; r++) {
        for (let c = 0; c < currentGrid.size; c++) {
            currentGrid.cells[r][c] = 'sea';
        }
    }

    renderGrid();
    clearResults();
    logToConsole('🔄 تم إعادة تعيين الشبكة إلى الحالة الافتراضية (كلها بحر)');
}

async function checkSolution() {
    if (currentGrid.size === 0) {
        alert('يرجى تحميل الشبكة أولاً');
        return;
    }

    const loading = document.getElementById('loading');
    const results = document.getElementById('results');
    const log = document.getElementById('log');
    const checkBtn = document.getElementById('check-btn');

    results.style.display = 'none';
    loading.style.display = 'block';
    checkBtn.disabled = true;
    clearInvalidHighlights();

    try {
        logToConsole('🔍 بدء التحقق من الحل...');
        const gridData = currentGrid.cells.map(row => 
            row.map(cell => cell === 'ship' ? 2 : 1)
        );

        const requestData = { grid: gridData };
        console.log('Sending validation request:', requestData);
        logToConsole('📤 إرسال البيانات للخادم للتحقق...');

        const response = await fetch(API_ENDPOINTS.validate, {
            method: 'POST',
            headers: { 
                'Content-Type': 'application/json; charset=UTF-8',
                'Accept': 'application/json; charset=UTF-8'
            },
            body: JSON.stringify(requestData)
        });

        const validationResult = await handleServerResponse(response);
        console.log('Validation response:', validationResult);

        loading.style.display = 'none';
        results.style.display = 'block';
        log.innerHTML = '';

        if (validationResult.valid) {
            const successMessage = validationResult.message || 'الحل صحيح ✅';
            log.innerHTML = `<div class="log-entry success">${successMessage}</div>`;
            logToConsole('✅ التحقق مكتمل: الحل صحيح');
            
            logToConsole('🎉 تهانينا! جميع القواعد محققة بنجاح');
        } else {
            const errorMessage = validationResult.message || 'الحل خاطئ ❌';
            log.innerHTML = `<div class="log-entry error">${errorMessage}</div>`;
            logToConsole('❌ التحقق مكتمل: الحل غير صحيح');

            if (validationResult.errors && Array.isArray(validationResult.errors)) {
                validationResult.errors.forEach(error => {
                    log.innerHTML += `<div class="log-entry error">• ${error}</div>`;
                    logToConsole(`❌ خطأ: ${error}`);
                });
            }

            if (validationResult.invalid_cells && Array.isArray(validationResult.invalid_cells)) {
                highlightInvalidCells(validationResult.invalid_cells);
            }
        }

    } catch (error) {
        loading.style.display = 'none';
        results.style.display = 'block';
        log.innerHTML = `<div class="log-entry error">❌ خطأ أثناء التحقق: ${error.message}</div>`;
        logToConsole('❌ خطأ أثناء التحقق: ' + error.message);
    } finally {
        checkBtn.disabled = false;
    }
}

function highlightInvalidCells(invalidCells) {
    const cells = document.querySelectorAll('.cell');
    invalidCells.forEach(([r, c]) => {
        const cell = document.querySelector(`[data-row="${r}"][data-col="${c}"]`);
        if (cell) {
            cell.classList.add('invalid');
        }
    });
}

function clearInvalidHighlights() {
    const cells = document.querySelectorAll('.cell.invalid');
    cells.forEach(cell => cell.classList.remove('invalid'));
}

function clearResults() {
    const results = document.getElementById('results');
    const log = document.getElementById('log');
    results.style.display = 'none';
    log.innerHTML = '';
    clearInvalidHighlights();
}

function logToConsole(text) {
    const consoleEl = document.getElementById('console');
    const timestamp = new Date().toLocaleTimeString('ar-SA');
    consoleEl.textContent += `\n[${timestamp}] ${text}`;
    consoleEl.scrollTop = consoleEl.scrollHeight;
}

window.addEventListener('DOMContentLoaded', () => {
    console.log('DOMContentLoaded fired');
    
    document.getElementById('load-btn').addEventListener('click', async () => {
        if (!(await checkServerStatus())) {
            alert('لا يمكن الاتصال بالخادم. تحقق من تشغيل الخادم.');
            return;
        }
        await loadGridFromServer();
    });

    document.getElementById('reset-btn').addEventListener('click', () => {
        resetGrid();
    });

    document.getElementById('check-btn').addEventListener('click', async () => {
        await checkSolution();
    });

    (async () => {
        logToConsole('🚀 بدء تشغيل مدقق معركة البحر...');
        if (await checkServerStatus()) {
            await loadGridFromServer();
        } else {
            logToConsole('⚠️ يرجى الضغط على "تحميل البيانات" بعد التأكد من تشغيل الخادم');
        }
    })();
});