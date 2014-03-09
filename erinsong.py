import music
import drums
import song

Tempo = 2.0
Sample_Rate = 44100

basspattern1 = '1   1   1   1   '

pattern1 = drums.Pattern(Sample_Rate, Tempo * 4, [basspattern1], [drums.bass])
pattern2 = drums.Pattern(Sample_Rate, Tempo * 4, [basspattern1], [drums.bass])
s = song.Song(Sample_Rate, Tempo,
              [(0.0, pattern1),
               (4.0, pattern2)])

music.s32outputfile('pysound.s32', music.gain(2**27, s))
