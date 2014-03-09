class Song(object):
    def __init__(self, sample_rate, tempo, sections):
        """ sections is a list of (start_time, signal) """
        self.sample_rate = sample_rate
        self.tempo = float(tempo)
        self.sections = sorted(sections)
        self.time = 0.0
        self.playing_sections = []

    def __iter__(self):
        return self

    def next(self):
        self.time += self.tempo / self.sample_rate

        while len(self.sections) > 0 and self.sections[0][0] <= self.time:
            print 'Starting section', self.sections[0]
            self.playing_sections.append(self.sections[0][1])
            del self.sections[0]

        i = 0
        sample = 0.0
        while i < len(self.playing_sections):
            try:
                sample += self.playing_sections[i].next()
                i = i + 1
            except StopIteration:
                print 'Stopping section', self.playing_sections[i]
                del self.playing_sections[i]

        if len(self.playing_sections) == 0 and len(self.sections) == 0:
            raise StopIteration()

        return sample
