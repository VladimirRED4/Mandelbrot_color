extern crate num;
use num::Complex;
use std::str::FromStr;

use std::io::Write;

extern crate image;
use image::{ColorType, Rgb};
use image::codecs::png::PngEncoder;
use image::ImageEncoder;
use std::fs::File;

extern crate crossbeam;

#[allow(dead_code)]
fn complex_square_add_loop(c: Complex<f64>) {
    let mut z = Complex {re: 0.0, im: 0.0};
    loop {
        z = z * z + c;
    }
}

/// Пытается определить, принадлежит ли `c` множеству Мандельброта, ограничившись
/// `limit` итерациями.
fn escape_time(c: Complex<f64>, limit: u32) -> Option<u32> {
    let mut z = Complex { re: 0.0, im: 0.0 };
    for i in 0..limit {
        z = z*z + c;
        if z.norm_sqr() > 4.0 {
            return Some(i);
        }
    }
    None
}

/// Цветовые схемы для раскрашивания
#[derive(Clone, Copy)]
enum ColorScheme {
    Classic,
    BlueRed,
    Rainbow,
    Fire,
    Ocean,
}

impl FromStr for ColorScheme {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "classic" => Ok(ColorScheme::Classic),
            "bluered" => Ok(ColorScheme::BlueRed),
            "rainbow" => Ok(ColorScheme::Rainbow),
            "fire" => Ok(ColorScheme::Fire),
            "ocean" => Ok(ColorScheme::Ocean),
            _ => Err(format!("Неизвестная цветовая схема: {}. Доступные: classic, bluered, rainbow, fire, ocean", s))
        }
    }
}

/// Преобразует количество итераций в цвет RGB
fn get_color(iterations: Option<u32>, max_iterations: u32, scheme: ColorScheme) -> Rgb<u8> {
    match iterations {
        None => Rgb([0, 0, 0]), // Черный для точек внутри множества
        Some(count) => {
            let t = count as f32 / max_iterations as f32;

            match scheme {
                ColorScheme::Classic => {
                    // Классическая схема - градиент от синего к желтому
                    let r = (t * 255.0) as u8;
                    let g = (t * 255.0) as u8;
                    let b = (255.0 - t * 255.0) as u8;
                    Rgb([r, g, b])
                }
                ColorScheme::BlueRed => {
                    // Сине-красный градиент
                    let r = (t * 255.0) as u8;
                    let g = 0;
                    let b = ((1.0 - t) * 255.0) as u8;
                    Rgb([r, g, b])
                }
                ColorScheme::Rainbow => {
                    // Радужная схема
                    let hue = t * 360.0;
                    let (r, g, b) = hsv_to_rgb(hue, 1.0, 1.0);
                    Rgb([r, g, b])
                }
                ColorScheme::Fire => {
                    // Огненная схема
                    let r = 255;
                    let g = (t * 255.0) as u8;
                    let b = (t * 128.0) as u8;
                    Rgb([r, g, b])
                }
                ColorScheme::Ocean => {
                    // Океанская схема
                    let r = 0;
                    let g = (t * 200.0) as u8;
                    let b = (128.0 + t * 127.0) as u8;
                    Rgb([r, g, b])
                }
            }
        }
    }
}

/// Преобразование HSV в RGB
fn hsv_to_rgb(h: f32, s: f32, v: f32) -> (u8, u8, u8) {
    let c = v * s;
    let x = c * (1.0 - ((h / 60.0) % 2.0 - 1.0).abs());
    let m = v - c;

    let (r, g, b) = if h < 60.0 {
        (c, x, 0.0)
    } else if h < 120.0 {
        (x, c, 0.0)
    } else if h < 180.0 {
        (0.0, c, x)
    } else if h < 240.0 {
        (0.0, x, c)
    } else if h < 300.0 {
        (x, 0.0, c)
    } else {
        (c, 0.0, x)
    };

    (
        ((r + m) * 255.0) as u8,
        ((g + m) * 255.0) as u8,
        ((b + m) * 255.0) as u8,
    )
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 6 {
        writeln!(std::io::stderr(), "Порядок вызова: mandelbrot FILE PIXELS UPPERLEFT LOWERRIGHT COLOR_SCHEME")
            .unwrap();
        writeln!(std::io::stderr(), "Пример: {} mandel.png 1000x750 -1.20,0.35 -1,0.20 rainbow", args[0])
            .unwrap();
        writeln!(std::io::stderr(), "Доступные цветовые схемы: classic, bluered, rainbow, fire, ocean")
            .unwrap();
        std::process::exit(1);
    }
    let bounds = parse_pair(&args[2], 'x')
        .expect("ошибка при разборе размеров изображения");
    let upper_left = parse_complex(&args[3])
        .expect("ошибка при разборе координат левого верхнего угла");
    let lower_right = parse_complex(&args[4])
        .expect("ошибка при разборе координат правого нижнего угла");

    // Получаем цветовую схему из аргументов командной строки
    let color_scheme = args[5].parse()
        .expect("ошибка при разборе цветовой схемы");

    let mut pixels = vec![0; bounds.0 * bounds.1 * 3]; // Теперь 3 байта на пиксель (RGB)

    // Многопоточная версия рендеринга
    let threads = 8;
    let rows_per_band = bounds.1 / threads + 1;
    {
        let bands: Vec<&mut [u8]> = pixels.chunks_mut(rows_per_band * bounds.0 * 3).collect();
        crossbeam::scope(|spawner| {
            for (i, band) in bands.into_iter().enumerate() {
                let top = rows_per_band * i;
                let height = band.len() / (bounds.0 * 3);
                let band_bounds = (bounds.0, height);
                let band_upper_left =
                    pixel_to_point(bounds, (0, top), upper_left, lower_right);
                let band_lower_right =
                    pixel_to_point(bounds, (bounds.0, top + height), upper_left, lower_right);
                spawner.spawn(move |_| {
                    render_color(band, band_bounds, band_upper_left, band_lower_right, color_scheme);
                });
            }
        }).unwrap();
    }

    write_image_color(&args[1], &pixels, bounds)
        .expect("ошибка при записи PNG-файла");
}

/// Разбирает строку `s`, содержащую пару координат
fn parse_pair<T: FromStr>(s: &str, separator: char) -> Option<(T, T)> {
    match s.find(separator) {
        None => None,
        Some(index) => {
            match (T::from_str(&s[..index]), T::from_str(&s[index + 1..])) {
                (Ok(l), Ok(r)) => Some((l, r)),
                _ => None
            }
        }
    }
}

#[test]
fn test_parse_pair() {
    assert_eq!(parse_pair::<i32>("",        ','), None);
    assert_eq!(parse_pair::<i32>("10,",     ','), None);
    assert_eq!(parse_pair::<i32>(",10",     ','), None);
    assert_eq!(parse_pair::<i32>("10,20",   ','), Some((10, 20)));
    assert_eq!(parse_pair::<i32>("10,20xy", ','), None);
    assert_eq!(parse_pair::<f64>("0.5x",    'x'), None);
    assert_eq!(parse_pair::<f64>("0.5x1.5", 'x'), Some((0.5, 1.5)));
 }

/// Разбирает пару чисел с плавающей точкой в комплексное число
fn parse_complex(s: &str) -> Option<Complex<f64>> {
    match parse_pair(s, ',') {
        Some((re, im)) => Some(Complex { re, im }),
        None => None
    }
 }

#[test]
fn test_parse_complex() {
    assert_eq!(parse_complex("1.25,-0.0625"), Some(Complex { re: 1.25, im: -0.0625 }));
    assert_eq!(parse_complex(",-0.0625"), None);
}

/// Преобразует координаты пикселя в точку на комплексной плоскости
fn pixel_to_point(bounds: (usize, usize),
                  pixel: (usize, usize),
                  upper_left: Complex<f64>,
                  lower_right: Complex<f64>) -> Complex<f64>
{
    let (width, height) = (lower_right.re - upper_left.re, upper_left.im - lower_right.im);
    Complex {
        re: upper_left.re + pixel.0 as f64 * width  / bounds.0 as f64,
        im: upper_left.im - pixel.1 as f64 * height / bounds.1 as f64
    }
}

#[test]
fn test_pixel_to_point() {
    assert_eq!(pixel_to_point((100, 100), (25, 75),
                                Complex { re: -1.0, im:  1.0 },
                                Complex { re:  1.0, im: -1.0 }),
                                Complex { re: -0.5, im: -0.5 });
}

/// Цветной рендеринг множества Мандельброта
fn render_color(pixels: &mut [u8],
                bounds: (usize, usize),
                upper_left: Complex<f64>,
                lower_right: Complex<f64>,
                color_scheme: ColorScheme)
{
    assert!(pixels.len() == bounds.0 * bounds.1 * 3);
    for row in 0..bounds.1 {
        for column in 0..bounds.0 {
            let point = pixel_to_point(bounds, (column, row), upper_left, lower_right);
            let color = get_color(escape_time(point, 255), 255, color_scheme);

            let pixel_index = (row * bounds.0 + column) * 3;
            pixels[pixel_index] = color[0];
            pixels[pixel_index + 1] = color[1];
            pixels[pixel_index + 2] = color[2];
        }
    }
}

/// Записывает цветное изображение в файл
fn write_image_color(filename: &str, pixels: &[u8], bounds: (usize, usize)) -> Result<(), Box<dyn std::error::Error>>
{
    let output = File::create(filename)?;
    let encoder = PngEncoder::new(output);
    encoder.write_image(&pixels,
                       bounds.0 as u32,
                       bounds.1 as u32,
                       ColorType::Rgb8)?;
    Ok(())
}