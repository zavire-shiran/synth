import struct
import math

def sine_wave_func(phase):
    return math.sin(phase * 2 * math.pi)

def square_wave_func(phase):
    if phase < 0.5:
        return 1.0
    else:
        return -1.0

def triangle_wave_func(phase):
    if phase < 0.25:
        return phase * 4
    elif phase < 0.75:
        return 1 - ((phase - 0.25) * 4)
    else:
        return (phase - 1) * 4

def null_signal():
    while True:
        yield 0.0

class Oscillator(object):
    def __init__(self, sample_rate, frequency, func):
        self.sample_rate = sample_rate
        self.frequency = float(frequency)
        self.func = func
        self.phase = 0.0

    def __iter__(self):
        return self

    def next(self):
        sample = self.func(self.phase)
        self.phase += self.frequency / self.sample_rate
        if self.phase > 1.0:
            self.phase -= 1.0
        return sample

class Instrument(object):
    def __init__(self, sample_rate, notes, func):
        self.sample_rate = sample_rate
        self.notes = sorted(notes)
        self.func = func
        self.time = 0.0
        self.playing_notes = []

    def __iter__(self):
        return self

    def next(self):
        self.time += 1.0 / self.sample_rate

        self.playing_notes.sort()
        while len(self.playing_notes) and self.playing_notes[0][0] <= self.time:
            del self.playing_notes[0]

        while len(self.notes) > 0 and self.notes[0][0] <= self.time:
            self.playing_notes.append((self.notes[0][1], self.func(*self.notes[0][2:])))
            del self.notes[0]

        if len(self.notes) + len(self.playing_notes) == 0:
            raise StopIteration()

        sample = 0.0

        for end, signal in self.playing_notes:
            try:
                sample += signal.next()
            except StopIteration:
                pass #we don't care if these signals run dry. They'll be culled eventually.

        return sample

def append_signals(*signals):
    for signal in signals:
        for sample in signal:
            yield sample

def delay_signal(sample_rate, seconds, signal):
    return append_signals(truncate_signal(sample_rate, seconds, null_signal()), signal)

def add_signals(*signals):
    while True:
        sample = 0.0
        running_signals = 0
        for s in signals:
            try:
                sample += s.next()
                running_signals += 1
            except StopIteration:
                pass
        if running_signals == 0:
            return
        yield sample

def s32outputfile(filename, signal_iter):
    f = file(filename, 'wb')
    for sample in signal_iter:
        output = struct.pack('i', sample)
        f.write(output)

def truncate_signal(sample_rate, seconds, signal):
    for i, sample in enumerate(signal):
        if i < (sample_rate * seconds):
            yield sample
        else:
            return

def gain(g, signal):
    for sample in signal:
        yield g * sample

def pitch2frequency(pitch):
    return 440 * (2 ** ((pitch - 69.0) / 12))

if __name__ == '__main__':
    notes = [[0, 0.5, 60],
             [0.5, 1, 62],
             [1, 1.5, 64],
             [1.5, 2, 65],
             [2, 2.5, 67],
             [2.5, 3, 69],
             [3, 3.5, 71],
             [3.5, 4, 72],
             [4, 5, 60],
             [4, 5, 64],
             [4, 5, 67],
             [4, 5, 72]]
    instr = Instrument(44100, notes, lambda f: Oscillator(44100, pitch2frequency(f), triangle_wave_func))
#    signals = [truncate_signal(44100, 1, Oscillator(44100, pitch2frequency(60), square_wave_func)),
#               delay_signal(44100, 1, truncate_signal(44100, 1, Oscillator(44100, pitch2frequency(64), square_wave_func))),
#               delay_signal(44100, 2, truncate_signal(44100, 1, Oscillator(44100, pitch2frequency(67), square_wave_func)))]
    s32outputfile('pysound.s32', gain(2**26, instr))
