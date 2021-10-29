import os
path = os.path.dirname(__file__)

with open(f'{path}/readme.txt', 'w') as f:
  f.write('readme')