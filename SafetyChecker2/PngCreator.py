# import related libraries
import os
from graphviz import Source 
from subprocess import check_call

def main():
    #check_call(['dot','-Tps','_all.dot','-o','_all.ps'])
    #check_call(['dot','-Tps','_error.dot','-o','_error.ps'])
    check_call(['dot','-Tps','_error1.dot','-o','_error1.ps'])
    #file = open('AllPaths.dot','r')
    #text = file.read()
    #print text
    #Source(text)
    

if __name__ == "__main__":
    main()
