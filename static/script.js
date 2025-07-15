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
                    currentGrid.cells[r][c] = cellData.type || 'sea';
                }
            }

            renderGrid();
            updateColumnRequirements();
            logToConsole('✅ تم تحميل البيانات بنجاح');
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

    container.style.gridTemplateColumns = `repeat(${currentGrid.size}, 60px)`;

    for (let i = 0; i < currentGrid.size; i++) {
        const div = document.createElement('div');
        div.className = 'req-cell';
        div.textContent = currentGrid.colRequirements[i];
        container.appendChild(div);
    }
}

function renderGrid() {
    const boardRows = document.getElementById('board-rows');
    boardRows.innerHTML = '';

    for (let r = 0; r < currentGrid.size; r++) {
        const rowDiv = document.createElement('div');
        rowDiv.className = 'board-row';

        const gridDiv = document.createElement('div');
        gridDiv.className = 'grid';
        gridDiv.style.gridTemplateColumns = `repeat(${currentGrid.size}, 60px)`;

        for (let c = 0; c < currentGrid.size; c++) {
            const cell = document.createElement('div');
            cell.className = 'cell';
            cell.textContent = currentGrid.cells[r][c] === 'ship' ? '🚢' : '🌊';
            cell.classList.add(currentGrid.cells[r][c] === 'ship' ? 'ship' : 'sea');


            gridDiv.appendChild(cell);
        }

        rowDiv.appendChild(gridDiv);

        const rowReqDiv = document.createElement('div');
        rowReqDiv.className = 'row-req';
        rowReqDiv.textContent = currentGrid.rowRequirements[r];
        rowDiv.appendChild(rowReqDiv);

        boardRows.appendChild(rowDiv);
    }
}

function resetGrid() {
    if (currentGrid.size === 0) return;

    for (let r = 0; r < currentGrid.size; r++) {
        for (let c = 0; c < currentGrid.size; c++) {
            currentGrid.cells[r][c] = 'sea';
        }
    }

    renderGrid();
    clearResults();
    logToConsole('✅ تم إعادة تعيين الشبكة إلى الحالة الافتراضية (كلها بحر)');
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
        } else {
            const errorMessage = validationResult.message || 'الحل خاطئ ❌';
            log.innerHTML = `<div class="log-entry error">${errorMessage}</div>`;
            logToConsole('❌ التحقق مكتمل: الحل غير صحيح');

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
    const boardRows = document.getElementById('board-rows');
    invalidCells.forEach(([r, c]) => {
        const rowDiv = boardRows.children[r];
        if (!rowDiv) return;

        const gridDiv = rowDiv.querySelector('.grid');
        if (!gridDiv) return;

        const cell = gridDiv.children[c];
        if (!cell) return;

        cell.classList.add('invalid');
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
    consoleEl.textContent += '\n' + text;
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
        if (await checkServerStatus()) {
            await loadGridFromServer();
        }
    })();
});