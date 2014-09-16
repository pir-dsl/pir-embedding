package edu.uwm.cs.pir.utils

import Constants._

case class ExecutionConfigTransmedia () {
  //CEDD feature env
  var scaleWidth = SCALE_WIDTH
  var scaleHeight = SCALE_HEIGHT
  //Clustering env
  var numberOfClusters = NUM_OF_CLUSTERS
  var clusterFilename = CLUSTER_FIlE
  //LDA env
  var ldaModelFile = LDA_MODEL_FIlE
  var descFile = EXTRACTED_TEXT_FILE_ROOT + "ALL_DESC.txt"
  var stopwordFile = STOPWORDS_ROOT + "en.txt"
  var numberOfTopics = NUM_OF_TOPICS
  var alphaSum = ALPHA_SUM
  var betaW = BETA_W
  var numberOfSamplers = NUMBER_SAMPLER
  var numberOfIterations = NUMBER_ITERATION
  var encoding = DEFAULT_ENCODING
  //LDA env
  var gibbsSamplingIteration = GIBBS_SAMPLING_ITERATION
  var gibbsSamplingThinning = GIBBS_SAMPLING_THINNING
  var gibbsSamplingBurnin = GIBBS_SAMPLING_BURNIN

  //CCA env
  var imageFeatureSize = NUM_OF_CLUSTERS
  var textFeatureSize =

    //TODO: Fix this
    //    if (awsS3Config.isIs_s3_storage()) {
    //    edu.uwm.cs.mir.prototypes.aws.AWSS3API.getNumberOfLinesOfS3Objects(awsS3Config, GROUND_TRUTH_CATEGORY_LIST, edu.uwm.cs.mir.prototypes.aws.AWSS3API.getAmazonS3Client(awsS3Config), false)
    //  } else {
    org.apache.commons.io.FileUtils.readLines(new java.io.File(GROUND_TRUTH_CATEGORY_LIST)).size
  //CCA Proj env
  var numberOfTopResults = 100
  var groungTruthcategories = GROUND_TRUTH_CATEGORY_LIST
}