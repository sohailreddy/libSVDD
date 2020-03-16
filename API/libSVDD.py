#!/usr/bin/python3

'''
    libSVDD: A Library for Support Vector Data Description
    Copyright (C) 2020 Sohail R. Reddy

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along
    with this program; if not, write to the Free Software Foundation, Inc.,
    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
'''


from ctypes import *
import numpy as np


class SVM_CLASS:

	def splitString(self,string):
		tmp = string.split()
		if len(tmp) == 1:
			return '', tmp[0], int(round(float(tmp[0]))), float(tmp[0])				
		else:
			return tmp[0], tmp[1], int(round(float(tmp[1]))), float(tmp[1])



	def initFromFile(self, file):
	
		tmpString = file.readline()
		if tmpString.rstrip() == '$End:Class':
			return
		elif len(tmpString) == 0:
			file.close()
			return
			
		string, sNum, iNum, fNum = self.splitString(file.readline()); self.label = sNum

		_, _, iNum, _ = self.splitString(file.readline()); n = iNum
		_, _, iNum, _ = self.splitString(file.readline()); m = iNum
		_, _, _, fNum = self.splitString(file.readline()); self.radius = np.array(fNum, dtype = c_double)
		_, _, _, fNum = self.splitString(file.readline()); self.C_x = np.array(fNum, dtype = c_double)
				
		string, _, iNum, _ = self.splitString(file.readline()); self.params = np.zeros(iNum, dtype = c_double)

		for i in range(0, iNum):
			_, _, _, fNum = self.splitString(file.readline()); self.params[i] = fNum
			
		self.params = np.delete(np.insert(self.params,0, 1.0e-10), -1)
		self.params = np.delete(np.insert(self.params,0, 0.25), -1)
			
		tmpString = file.readline()
		tmpString = file.readline()


		self.supportVectors = np.zeros((n,m), dtype = c_double)
		self.Lagrange = np.zeros(n, dtype = c_double)
		
		for i in range(0,n):
			tmpString = np.array(file.readline().split(), dtype = c_double)
			self.supportVectors[i,:] = tmpString[0:m]
			self.Lagrange[i] = tmpString[m]
	
		tmpString = file.readline()
		tmpString = file.readline()

		self.center = np.matmul(self.supportVectors.T, self.Lagrange)
				
		if tmpString.rstrip() == '$End:Class':
			return
		


	def __init__(self, objects = [], label = '-1', **kwargs):


		initFile = kwargs.get('init', '')		
		if initFile == '' and objects == []:
			print('no objects or initialization file to construct SVDD')
			return
		
		
		libSVDDPath = kwargs.get('libSVDD', '')		
		self.libSVDD = cdll.LoadLibrary('./'+libSVDDPath+'libSVDD.so')
		
		if initFile != '':
			self.initFromFile(initFile)
			return

		
		self.libSVDD.train.argtypes = [POINTER(c_int) ,		# m
							   POINTER(c_int) ,		# n
							   POINTER(c_double) ,	# objects
							   POINTER(c_double),	# params
							   POINTER(c_char),		# label
							   POINTER(c_double),	# center
							   POINTER(c_double),	# radius
							   POINTER(c_double),	# Lagrange
							   POINTER(c_double), 	# C_x term
							   POINTER(c_char)	]	# filename

		n = objects.shape[0]
		m = objects.shape[1]


		self.label = label
		self.params = np.zeros((20), dtype = c_double)


		self.params[0] = kwargs.get('tol', 1.0e-10)		# tolerance
		self.params[1] = kwargs.get('slack', 0.25)		# max bound for Lagrange
		self.params[2] = kwargs.get('kernel', 1)		# type of kernel
		self.params[3] = kwargs.get('sigma', 1.0)		# sigma for gau kernel
		self.params[4] = kwargs.get('c0', 0.0)		# sigma for gau kernel
		self.params[5] = kwargs.get('gamma', 1.0)		# sigma for gau kernel
		self.params[6] = kwargs.get('d', 2.0)		# sigma for gau kernel

		
		
		self.center = np.zeros((m), dtype = c_double)		
		self.radius = np.zeros((1), dtype = c_double)		
		Lagrange = np.zeros((n), dtype = c_double)
		self.C_x = np.zeros((1), dtype = c_double)		
								
		svmObjects = objects.ctypes.data_as(POINTER(c_double))
		svmparams = self.params.ctypes.data_as(POINTER(c_double))
		svmCenter = self.center.ctypes.data_as(POINTER(c_double))
		svmRadius = self.radius.ctypes.data_as(POINTER(c_double))
		svmLagrange = Lagrange.ctypes.data_as(POINTER(c_double))
		svmC_x = self.C_x.ctypes.data_as(POINTER(c_double))
		svmLabel = np.asarray(label + '\0', dtype = c_char).ctypes.data_as(POINTER(c_char))
		
		tmp = np.asarray(kwargs.get('save', '') + '\0', dtype = c_char)		
		svmSaveToFile= tmp.ctypes.data_as(POINTER(c_char))
		
		self.libSVDD.train( c_int(n), c_int(m), svmObjects, svmparams, svmLabel,
					svmCenter, svmRadius, svmLagrange, svmC_x, svmSaveToFile )

		self.supportVectors = objects[Lagrange > self.params[0]]
		self.Lagrange = Lagrange[Lagrange > self.params[0]]


	def classify(self, objects):
	

		self.libSVDD.classify.argtypes=[POINTER(c_int) ,	# m
							   POINTER(c_int) ,		# n
							   POINTER(c_double) ,	# support vector
							   POINTER(c_double),	# params
							   POINTER(c_double),	# center
							   POINTER(c_double),	# radius
							   POINTER(c_double),	# Lagrange
							   POINTER(c_double), 	# C_x term
							   POINTER(c_double),	# object to classify
							   POINTER(c_double),	# object radius
							   POINTER(c_int)]		# accept or not


		radius = np.zeros(objects.shape[0])
		accept = np.zeros(objects.shape[0],  dtype = int)

		n = self.supportVectors.shape[0]
		m = self.supportVectors.shape[1]

		svmSV = self.supportVectors.ctypes.data_as(POINTER(c_double))
		svmparams = self.params.ctypes.data_as(POINTER(c_double))
		svmCenter = self.center.ctypes.data_as(POINTER(c_double))
		svmRadius = self.radius.ctypes.data_as(POINTER(c_double))
		svmLagrange = self.Lagrange.ctypes.data_as(POINTER(c_double))
		svmC_x = self.C_x.ctypes.data_as(POINTER(c_double))
		
		a = np.zeros((1), dtype = c_int)
		r = np.zeros((1), dtype = c_double)
						
		for i in range(0, objects.shape[0]):
		
			object = np.array(objects[i,:], dtype = c_double)
			
			svmObject = object.ctypes.data_as(POINTER(c_double))
			svmObjectRadius = r.ctypes.data_as(POINTER(c_double))
			svmAccept = a.ctypes.data_as(POINTER(c_int))

			self.libSVDD.classify( c_int(n), c_int(m), svmSV, svmparams, 
						   svmCenter, svmRadius, svmLagrange, svmC_x, 
						   svmObject, svmObjectRadius, svmAccept )

			radius[i] = r
			accept[i] = a

		return radius, accept
	
	
	

class SVM:


	def initFromFile(self, maxClasses = 1000, **kwargs,):

		filename = kwargs.get('init', '')
		libSVDD = kwargs.get('libSVDD', '')			
		initFile = open(filename, 'r')
		self.classes = []
		for i in range(0, maxClasses):
			if initFile.closed == False:
				self.classes.append( SVM_CLASS( init = initFile, libSVDD = libSVDD ) )
			
		self.nClasses = len(self.classes) - 1
		
		if self.nClasses == 1:
			self.classes = self.classes[0]
		else:
			self.classes = self.classes[0:self.nClasses]			




	def __init__(self, objects = [], labels = '-1', **kwargs):

		initFile = kwargs.get('init', '')

		if initFile == '' and objects == []:
			print('no objects or initialization file to construct SVDD')
			return


		if initFile != '':
			self.initFromFile(**kwargs)
			return

		if objects.ndim == 1:
			objects = np.reshape(objects, (objects.shape[0],1))

		saveFile = kwargs.get('save', '')
		if saveFile != '':
			open(saveFile, 'w').close()

		uniqueLabels = np.unique(labels)
		self.nClasses = len(uniqueLabels)

		if self.nClasses <= 1 :
			self.classes = SVM_CLASS( objects, uniqueLabels[0], **kwargs )
		else:
			self.classes = []
			for i in range(0, self.nClasses):
				self.classes.append( SVM_CLASS( objects[labels == uniqueLabels[i]], 
												uniqueLabels[i], **kwargs ) )


		
	def classify(self, objects):
			
		if objects.ndim == 1:
			objects = np.reshape(objects, (1,objects.shape[0]))

		n = objects.shape[0]		
		radius = np.ones(n) * 987654321.0
		labels = np.zeros(n,  dtype = str)
		labels[:] = ''

		if self.nClasses == 1:
			radius, accept = self.classes.classify(objects)
			labels[accept == 1] = self.classes.label

		else:		
			for i in range(0, self.nClasses):
				tmpRadius, tmpAccept = self.classes[i].classify(objects)
#				radius[ tmpAccept == 1 ] = tmpRadius[tmpAccept == 1]

				labels[ radius > tmpRadius ] = self.classes[i].label
				radius[labels == self.classes[i].label] = tmpRadius[labels == self.classes[i].label]								
		
		if objects.shape[0] == 1:
			radius = radius[0]
			labels = labels[0]
		
		
		return radius, labels



