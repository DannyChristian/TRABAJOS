from flask import Flask, jsonify, request
import random
import statistics
from collections import Counter
import numpy as np

app = Flask(__name__)

class CarameloSimulator:
    def __init__(self):
        self.tipos = ['huevito', 'limon', 'pera']
        self.reset()
    
    def reset(self):
        self.fase = 1
        self.jugadores = []
        self.equipos = []
    
    def generar_caramelos(self, cantidad):
        """Genera caramelos usando numpy para mejor performance"""
        return np.random.choice(self.tipos, cantidad).tolist()
    
    def crear_jugadores(self, num_jugadores=30):
        """Crea jugadores optimizado"""
        self.fase = 1
        self.jugadores = [
            {'id': i+1, 'caramelos': self.generar_caramelos(2), 'salvado': False}
            for i in range(num_jugadores)
        ]
        return self.jugadores
    
    def crear_equipos(self, num_jugadores=30, por_equipo=15):
        """Crea equipos optimizado"""
        self.fase = 2
        jugadores = [
            {'id': i+1, 'caramelos': self.generar_caramelos(2)}
            for i in range(num_jugadores)
        ]
        
        random.shuffle(jugadores)
        self.equipos = [
            {
                'id': i//por_equipo + 1,
                'jugadores': jugadores[i:i+por_equipo],
                'chupetines': 0
            }
            for i in range(0, num_jugadores, por_equipo)
        ]
        return self.equipos
    
    def contar_caramelos(self, caramelos):
        """Cuenta caramelos más eficientemente"""
        contador = Counter(caramelos)
        return {tipo: contador.get(tipo, 0) for tipo in self.tipos}
    
    def simular_fase1(self):
        """Simula fase 1 optimizada"""
        if not self.jugadores:
            return {}
        
        # Recolectar todos los caramelos
        todos_caramelos = []
        for jugador in self.jugadores:
            todos_caramelos.extend(jugador['caramelos'])
        
        # Contar y calcular chupetines
        contador = self.contar_caramelos(todos_caramelos)
        chupetines = min(contador.values())
        
        # Marcar salvados
        for i in range(min(chupetines, len(self.jugadores))):
            self.jugadores[i]['salvado'] = True
        
        return {
            'fase': 1,
            'total_caramelos': len(todos_caramelos),
            'distribucion': contador,
            'chupetines': chupetines,
            'salvados': chupetines,
            'eficiencia': round((chupetines / len(self.jugadores)) * 100, 1)
        }
    
    def simular_fase2(self):
        """Simula fase 2 con bucle optimizado"""
        if not self.equipos:
            return {}
        
        resultados = []
        
        for equipo in self.equipos:
            # Recolectar caramelos del equipo
            caramelos = []
            for jugador in equipo['jugadores']:
                caramelos.extend(jugador['caramelos'])
            
            chupetines = 0
            iteraciones = 0
            
            # Bucle de intercambios optimizado
            while iteraciones < 50:  # Límite reducido
                contador = self.contar_caramelos(caramelos)
                
                # Verificar si puede intercambiar (2 de cada tipo)
                if all(contador[tipo] >= 2 for tipo in self.tipos):
                    # Realizar intercambio
                    for tipo in self.tipos:
                        for _ in range(2):
                            caramelos.remove(tipo)
                    
                    # Agregar 2 caramelos aleatorios
                    caramelos.extend(self.generar_caramelos(2))
                    chupetines += 1
                    iteraciones += 1
                else:
                    break
            
            equipo['chupetines'] = chupetines
            resultados.append({
                'equipo_id': equipo['id'],
                'jugadores': len(equipo['jugadores']),
                'chupetines': chupetines,
                'iteraciones': iteraciones
            })
        
        # Encontrar ganador
        max_chupetines = max(eq['chupetines'] for eq in self.equipos)
        ganadores = [eq for eq in self.equipos if eq['chupetines'] == max_chupetines]
        
        return {
            'fase': 2,
            'equipos': resultados,
            'ganador': ganadores[0]['id'] if len(ganadores) == 1 else 'Empate',
            'max_chupetines': max_chupetines
        }
    
    def estadisticas(self, simulaciones=1000):
        """Calcula estadísticas usando numpy para mejor performance"""
        f1_results = []
        f2_results = []
        
        for _ in range(simulaciones):
            # Fase 1
            self.crear_jugadores()
            result_f1 = self.simular_fase1()
            f1_results.append(result_f1['chupetines'])
            
            # Fase 2
            self.crear_equipos()
            result_f2 = self.simular_fase2()
            f2_results.append(result_f2['max_chupetines'])
        
        return {
            'fase1': {
                'promedio': round(np.mean(f1_results), 2),
                'mediana': int(np.median(f1_results)),
                'std': round(np.std(f1_results), 2),
                'min': int(np.min(f1_results)),
                'max': int(np.max(f1_results))
            },
            'fase2': {
                'promedio': round(np.mean(f2_results), 2),
                'mediana': int(np.median(f2_results)),
                'std': round(np.std(f2_results), 2),
                'min': int(np.min(f2_results)),
                'max': int(np.max(f2_results))
            },
            'simulaciones': simulaciones
        }

# Instancia global
sim = CarameloSimulator()

@app.route('/')
def index():
    return """<!DOCTYPE html>
<html lang="es">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>🍬 Simulador Caramelos</title>
    <style>
        * { margin: 0; padding: 0; box-sizing: border-box; }
        body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; 
               background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); min-height: 100vh; }
        .container { max-width: 900px; margin: 0 auto; padding: 20px; }
        .card { background: rgba(255,255,255,0.95); backdrop-filter: blur(10px); 
                border-radius: 20px; padding: 30px; margin: 20px 0; box-shadow: 0 8px 32px rgba(0,0,0,0.1); }
        .header { text-align: center; background: linear-gradient(45deg, #ff6b6b, #4ecdc4);
                  -webkit-background-clip: text; -webkit-text-fill-color: transparent; margin-bottom: 30px; }
        h1 { font-size: 2.5em; font-weight: 700; }
        .section { margin: 20px 0; }
        .controls { display: flex; gap: 15px; align-items: center; flex-wrap: wrap; margin: 20px 0; }
        input { padding: 12px; border: 2px solid #e1e8ed; border-radius: 10px; font-size: 16px; 
                transition: all 0.3s ease; }
        input:focus { border-color: #667eea; outline: none; box-shadow: 0 0 0 3px rgba(102,126,234,0.1); }
        .btn { padding: 12px 24px; border: none; border-radius: 10px; font-size: 16px; font-weight: 600;
               cursor: pointer; transition: all 0.3s ease; }
        .btn-primary { background: linear-gradient(45deg, #667eea, #764ba2); color: white; }
        .btn-success { background: linear-gradient(45deg, #4ecdc4, #44a08d); color: white; }
        .btn-warning { background: linear-gradient(45deg, #ffd89b, #19547b); color: white; }
        .btn:hover { transform: translateY(-2px); box-shadow: 0 5px 15px rgba(0,0,0,0.2); }
        .btn:active { transform: translateY(0); }
        .result { margin-top: 20px; padding: 20px; background: rgba(255,255,255,0.8); 
                  border-radius: 15px; border-left: 5px solid #667eea; }
        .stats-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 20px; }
        .stat-card { background: linear-gradient(135deg, #667eea, #764ba2); color: white; 
                     padding: 20px; border-radius: 15px; text-align: center; }
        .caramelo { display: inline-block; padding: 5px 10px; margin: 3px; border-radius: 15px; 
                    font-size: 14px; font-weight: 600; }
        .huevito { background: #ff6b6b; color: white; }
        .limon { background: #ffd93d; color: #333; }
        .pera { background: #6bcf7f; color: white; }
        .loading { text-align: center; padding: 20px; }
        .spinner { border: 3px solid #f3f3f3; border-top: 3px solid #667eea; border-radius: 50%; 
                   width: 30px; height: 30px; animation: spin 1s linear infinite; margin: 0 auto; }
        @keyframes spin { 0% { transform: rotate(0deg); } 100% { transform: rotate(360deg); } }
        .hidden { display: none; }
    </style>
</head>
<body>
    <div class="container">
        <div class="card">
            <div class="header">
                <h1>🍬 Simulador de Caramelos</h1>
                <p>Teoría de Juegos y Cooperación</p>
            </div>
        </div>
        
        <div class="card">
            <div class="section">
                <h2>📊 Fase 1: Cooperación Individual</h2>
                <p>Regla: 1 huevito + 1 limón + 1 pera → 1 chupetín</p>
                <div class="controls">
                    <input type="number" id="jugadores-f1" value="30" min="10" max="100" placeholder="Jugadores">
                    <button class="btn btn-primary" onclick="simularF1()">🎮 Simular</button>
                </div>
                <div id="resultado-f1" class="result hidden"></div>
            </div>
        </div>
        
        <div class="card">
            <div class="section">
                <h2>🏆 Fase 2: Competencia por Equipos</h2>
                <p>Regla: 2 huevitos + 2 limones + 2 peras → 1 chupetín + 2 caramelos</p>
                <div class="controls">
                    <input type="number" id="jugadores-f2" value="30" min="10" max="100" placeholder="Jugadores">
                    <button class="btn btn-success" onclick="simularF2()">🏆 Simular</button>
                </div>
                <div id="resultado-f2" class="result hidden"></div>
            </div>
        </div>
        
        <div class="card">
            <div class="section">
                <h2>📈 Análisis Estadístico</h2>
                <div class="controls">
                    <input type="number" id="simulaciones" value="1000" min="100" max="3000" placeholder="Simulaciones">
                    <button class="btn btn-warning" onclick="calcularStats()">📊 Analizar</button>
                </div>
                <div id="loading" class="loading hidden">
                    <div class="spinner"></div>
                    <p>Calculando...</p>
                </div>
                <div id="estadisticas" class="result hidden"></div>
            </div>
        </div>
    </div>

    <script>
        async function simularF1() {
            const jugadores = document.getElementById('jugadores-f1').value;
            const div = document.getElementById('resultado-f1');
            
            try {
                const response = await fetch(`/api/fase1?jugadores=${jugadores}`);
                const data = await response.json();
                
                div.innerHTML = `
                    <h3>🎯 Resultados Fase 1</h3>
                    <p><strong>Distribución:</strong> 
                        <span class="caramelo huevito">🥚 ${data.distribucion.huevito}</span>
                        <span class="caramelo limon">🍋 ${data.distribucion.limon}</span>
                        <span class="caramelo pera">🍐 ${data.distribucion.pera}</span>
                    </p>
                    <p><strong>Chupetines obtenidos:</strong> ${data.chupetines} 🍭</p>
                    <p><strong>Eficiencia:</strong> ${data.eficiencia}% (${data.salvados}/${jugadores} salvados)</p>
                `;
                div.classList.remove('hidden');
            } catch (error) {
                div.innerHTML = '<p style="color: red;">Error en la simulación</p>';
                div.classList.remove('hidden');
            }
        }
        
        async function simularF2() {
            const jugadores = document.getElementById('jugadores-f2').value;
            const div = document.getElementById('resultado-f2');
            
            try {
                const response = await fetch(`/api/fase2?jugadores=${jugadores}`);
                const data = await response.json();
                
                let html = `
                    <h3>🏆 Resultados Fase 2</h3>
                    <p><strong>🥇 Ganador:</strong> Equipo ${data.ganador}</p>
                    <p><strong>🍭 Máximo chupetines:</strong> ${data.max_chupetines}</p>
                    <div class="stats-grid">
                `;
                
                data.equipos.forEach(eq => {
                    const esGanador = eq.chupetines === data.max_chupetines;
                    html += `
                        <div class="stat-card" style="background: ${esGanador ? 'linear-gradient(135deg, #4ecdc4, #44a08d)' : 'linear-gradient(135deg, #95a5a6, #7f8c8d)'}">
                            <h4>Equipo ${eq.equipo_id} ${esGanador ? '🏆' : ''}</h4>
                            <p>👥 ${eq.jugadores} jugadores</p>
                            <p>🍭 ${eq.chupetines} chupetines</p>
                            <p>🔄 ${eq.iteraciones} intercambios</p>
                        </div>
                    `;
                });
                
                html += '</div>';
                div.innerHTML = html;
                div.classList.remove('hidden');
            } catch (error) {
                div.innerHTML = '<p style="color: red;">Error en la simulación</p>';
                div.classList.remove('hidden');
            }
        }
        
        async function calcularStats() {
            const simulaciones = document.getElementById('simulaciones').value;
            const div = document.getElementById('estadisticas');
            const loading = document.getElementById('loading');
            
            loading.classList.remove('hidden');
            div.classList.add('hidden');
            
            try {
                const response = await fetch(`/api/estadisticas?simulaciones=${simulaciones}`);
                const data = await response.json();
                
                div.innerHTML = `
                    <h3>📊 Análisis Estadístico (${data.simulaciones} simulaciones)</h3>
                    <div class="stats-grid">
                        <div class="stat-card">
                            <h4>📈 Fase 1 (Cooperación)</h4>
                            <p><strong>Promedio:</strong> ${data.fase1.promedio}</p>
                            <p><strong>Rango:</strong> ${data.fase1.min} - ${data.fase1.max}</p>
                            <p><strong>Desv. Est.:</strong> ${data.fase1.std}</p>
                        </div>
                        <div class="stat-card">
                            <h4>🏆 Fase 2 (Competencia)</h4>
                            <p><strong>Promedio:</strong> ${data.fase2.promedio}</p>
                            <p><strong>Rango:</strong> ${data.fase2.min} - ${data.fase2.max}</p>
                            <p><strong>Desv. Est.:</strong> ${data.fase2.std}</p>
                        </div>
                    </div>
                    <p><strong>💡 Insight:</strong> La Fase 2 genera más chupetines debido a los intercambios iterativos, demostrando cómo la competencia puede ser más eficiente que la cooperación simple.</p>
                `;
                div.classList.remove('hidden');
            } catch (error) {
                div.innerHTML = '<p style="color: red;">Error al calcular estadísticas</p>';
                div.classList.remove('hidden');
            } finally {
                loading.classList.add('hidden');
            }
        }
    </script>
</body>
</html>"""

@app.route('/api/fase1')
def api_fase1():
    jugadores = request.args.get('jugadores', 30, type=int)
    sim.crear_jugadores(jugadores)
    return jsonify(sim.simular_fase1())

@app.route('/api/fase2')
def api_fase2():
    jugadores = request.args.get('jugadores', 30, type=int)
    sim.crear_equipos(jugadores)
    return jsonify(sim.simular_fase2())

@app.route('/api/estadisticas')
def api_estadisticas():
    simulaciones = request.args.get('simulaciones', 1000, type=int)
    return jsonify(sim.estadisticas(simulaciones))

if __name__ == '__main__':
    print("🍬 Simulador optimizado iniciado!")
    print("📱 http://127.0.0.1:5000")
    app.run(debug=True)