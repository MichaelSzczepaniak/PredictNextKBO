import numpy as np, pandas as pd

def make_2d_grid(x_start, x_end, x_step, y_start, y_end, y_step,
                 x_name, y_name, z_name, z_series=None, outfile='grid.csv',
                 write_output=True) :
    """ Returns a dataframe of x and y grid values and an associated
    z value.
    """
    x_count = 1 + round((x_end - x_start) / x_step)
    y_count = 1 + round((y_end - y_start) / y_step)
    x_series = np.linspace(x_start, x_end, x_count)
    y_series = np.linspace(x_start, x_end, y_count)
    if z_series is None:
        z_series = [0] * (x_count * y_count)
    
    xvals = []
    yvals = []
    for x in x_series:
        xvals.extend([x] * x_count)
        yvals.extend(y_series)
    
    # x = (x_count, y_count, len(xvals), len(yvals), len(z_series))
    # print(x)
    result = pd.DataFrame({x_name:xvals, y_name:yvals, z_name:z_series})
    if write_output:
        result.to_csv(outfile, columns=('gamma2', 'gamma3', 'acc'), \
                      index=False)
    
    return result

# Test the function:
# eg.make_2d_grid(0.1, 1.9, 0.1, 0.1, 1.9, 0.1, 'gamma2', 'gamma3', 'acc')

def expand_togrid(dir='D:/Dropbox/sw_dev/projects/PredictNextKBO/cv/validation',
                  files=('cv_blogs_fold1_itrs500', 'cv_blogs_fold2_itrs500',
                         'cv_blogs_fold3_itrs500', 'cv_blogs_fold4_itrs500',
                         'cv_blogs_fold4_itrs500'), file_ext='.csv',
                  x_start = 0.1, x_end = 1.9, x_step = 0.1,
                  y_start = 0.1, y_end = 1.9, y_step = 0.1) :
    xy_grid =  make_2d_grid(0.1, 1.9, 0.1, 0.1, 1.9, 0.1, 'gamma2', 'gamma3',
                            'acc', write_output=False)
    for file in files:
        file_path = dir + '/' + file + file_ext
        df = pd.read_csv(file_path)
        for i in range(df.shape[0]):
            print(df['gamma3'].iloc[i])
    
    
    
    
    