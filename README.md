# randomart
![1](https://github.com/user-attachments/assets/a03a2cde-a74c-4dc7-86ad-0d1b4ff4e317)
![2](https://github.com/user-attachments/assets/053118e3-126a-48ee-95d5-ce9b003443b1)
![3](https://github.com/user-attachments/assets/9b9cfeee-49ad-465b-b860-9352360408a7)
![6](https://github.com/user-attachments/assets/fde688fa-8a94-4833-91ac-8eb64834c26e)


Implementation of RandomArt based on the algorithm described in [Hash Visualization: a New Technique to improve Real-World Security](https://users.ece.cmu.edu/~adrian/projects/validation/validation.pdf) and Andrej Bauer's original program.

To get started, make sure you have cabal installed. Cabal comes with GHCup, which you can download [here](https://www.haskell.org/ghcup/). Then clone and run:
```
git clone https://github.com/jamesma100/randomart.git
cd randomart
cabal run randomart -- [-d <depth>] [-p <pixels>] [-o <filepath>] [-s <seed>]
```

where
- `<depth>`: depth of randomly generated AST used to render your image
- `<pixels>`: width/height of the image, e.g. 200 means 200x200 pixels. Currently only square images are supported.
- `<filepath>`: path of image to be saved, e.g. "my_img.png"
- `<seed>`: most important part, any random string to initialize the random number generator

For example:
```
cabal run randomart -- -d 30 -p 200 -o ./my_img.png -s its-2025-and-the-world-is-ending
```
Then open `./my_img.png`. You can alter the seed and depth to generate different images.
