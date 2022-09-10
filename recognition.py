# import libraries
import speech_recognition as sr
import codecs
import glob
import os
from pydub import AudioSegment
from pydub.silence import split_on_silence

# recorgniser
r = sr.Recognizer()

# read audio

def get_large_audio_transcription(path):

    sound = AudioSegment.from_wav(path)  
    chunks = split_on_silence(sound,
        min_silence_len = 500,
        silence_thresh = sound.dBFS-20,
        keep_silence=500,
    )
    folder_name = "audio-chunks_{}".format(os.path.basename(file))
    # directory for audio chunks
    if not os.path.isdir(folder_name):
        os.mkdir(folder_name)
    transcription = ""
    # process chunks 
    for i, audio_chunk in enumerate(chunks, start=1):
        chunk_filename = os.path.join(folder_name, f"chunk{i}.wav")
        audio_chunk.export(chunk_filename, format="wav")
        with sr.AudioFile(chunk_filename) as source:
            audio_listened = r.listen(source)
            # transcribe chunk
            try:
   # insert language below where it says "hu-HU"            
                text = r.recognize_google(audio_listened, language="hu-HU")
            except sr.UnknownValueError as e:
                print("Error:", str(e))
            else:
                text = f"{text} "
                print(chunk_filename, ":", text)
                transcription += text
    with codecs.open("{}.txt".format(os.path.basename(os.path.splitext(file)[0])), "w", "utf-8") as f:
        f.write(transcription)
    return transcription

# use, insert path in <PATH> variable
files = (glob.glob(<PATH>))
print(files)
for file in files:
    get_large_audio_transcription(file)
