# 🎨 Визуализатор множества Мандельброта

![Mandelbrot Set](https://img.shields.io/badge/Fractal-Mandelbrot-blueviolet)
![Rust](https://img.shields.io/badge/Language-Rust-orange)
![Performance](https://img.shields.io/badge/Performance-High--Speed-red)

Программа для генерации и визуализации множества Мандельброта с поддержкой различных цветовых схем.

## 🚀 Быстрый старт

### Сборка и запуск

```bash
# Сборка в режиме релиза для максимальной производительности
cargo build --release

# Запуск с параметрами по умолчанию
time target/release/mandelbrot mandel.png 4000x3000 -1.20,0.35 -1,0.20 bluered
'''

'''text
# Параметры команды
mandelbrot <output_file> <resolution> <start_point> <end_point> <color_scheme>

output_file - имя выходного PNG файла

resolution - разрешение изображения (например: 4000x3000)

start_point - начальная точка комплексной плоскости (формат: x,y)

end_point - конечная точка комплексной плоскости (формат: x,y)

color_scheme - цветовая схема
