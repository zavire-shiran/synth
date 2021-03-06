import music
import song

SampleRate = 44100
Tempo = 1.0

notes_m1_3 = [[0, 0.3, 'F#5'],
              [0.33, 0.64, 'A#5'],
              [0.67, 0.97, 'C#6'],
              [1.0, 1.3, 'A#5'],
              [1.33, 1.63, 'F#5'],
              [1.67, 1.97, 'C#6'],
              [2.0, 4.3, 'C#6'], # This should be a trill
              [2.0, 2.3, 'F#4'],
              [2.33, 2.64, 'A#4'],
              [2.67, 2.97, 'C#5'],
              [3.0, 3.3, 'A#4'],
              [3.33, 3.64, 'F#4'],
              [3.67, 3.97, 'D#5'],
              [4.0, 4.95, 'D#5'],
              [4.33, 4.64, 'A#5'],
              [4.67, 5.3, 'B5'],
              [5.0, 5.95, 'C#5'],
              [5.33, 5.64, 'C#6'],
              [5.67, 6.3, 'A#5'],
              [6.0, 6.95, 'B4'],
              [6.33, 6.64, 'D#6'],
              [6.67, 7.3, 'G#5'],
              [7.0, 7.95, 'A#4'],
              [7.33, 7.64, 'A#5'],
              [7.67, 8.3, 'F#5'],
              [8.0, 8.95, 'G#4'],
              [8.33, 8.64, 'B5'],
              [8.67, 9.33, 'E#5'],
              [9.0, 9.95, 'F#4'],
              [9.33, 9.64, 'B5'],
              [9.67, 10.3, 'D#5'],
              [10.0, 10.95, 'E#4'],
              [10.33, 10.64, 'B5'],
              [10.67, 10.97, 'C#5'],
              [11, 11.95, 'C#4'],
              [11, 11.3, 'E#5'],
              [11.33, 11.64, 'G#5'],
              [11.67, 11.97, 'B5']]

instr_m1_3 = music.Instrument(SampleRate, Tempo, notes_m1_3, music.triangle_oscillator_instr_func)

song = song.Song(SampleRate, Tempo,
            [(0.0, instr_m1_3)])

music.s32outputfile('pysound.s32', music.gain(2**27, song))
