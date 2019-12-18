package opmodes.LM1;

import team25core.DeadReckonPath;

/**
 * Created by Lizzie on 11/5/2019.
 */
public class VivaldiSkybridgePath {
    public enum AllianceColor {
        BLUE,
        RED,
        DEFAULT;
    }

    public enum StartingPosition {
        BUILDING,
        LOADING,
        DEFAULT;
    }

    DeadReckonPath[][] paths = new DeadReckonPath[2][2];

    public VivaldiSkybridgePath() {
        // BLUE ALLIANCE, BUILDING SIDE
        paths[VivaldiSkybridgePath.AllianceColor.BLUE.ordinal()][VivaldiSkybridgePath.StartingPosition.BUILDING.ordinal()] = new DeadReckonPath();

        //latch to foundation
        paths[VivaldiSkybridgePath.AllianceColor.BLUE.ordinal()][VivaldiSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 20, 0.4); //sideways left to latch, 12
        paths[VivaldiSkybridgePath.AllianceColor.BLUE.ordinal()][VivaldiSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 12, -0.4); //backwards with foundation, 12
        paths[VivaldiSkybridgePath.AllianceColor.BLUE.ordinal()][VivaldiSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 12, -0.8); //forward to latch, 12
        paths[VivaldiSkybridgePath.AllianceColor.BLUE.ordinal()][VivaldiSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 90, 0.2); //turning foundation, 12
        paths[VivaldiSkybridgePath.AllianceColor.BLUE.ordinal()][VivaldiSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 20, 0.4); //forward to move foundation, 20
        paths[VivaldiSkybridgePath.AllianceColor.BLUE.ordinal()][VivaldiSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 12, -0.4);  //backward to park, 12
        paths[VivaldiSkybridgePath.AllianceColor.BLUE.ordinal()][VivaldiSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, -180, 0.4); //turn right to park, 12
        paths[VivaldiSkybridgePath.AllianceColor.BLUE.ordinal()][VivaldiSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 12, 0.4); //sideways right to park, 12

       // paths[VivaldiSkybridgePath.AllianceColor.BLUE.ordinal()][VivaldiSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 20, 0.4); //forward to latch, 12
       // paths[VivaldiSkybridgePath.AllianceColor.BLUE.ordinal()][VivaldiSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 12, -0.4); //sideways to latch, 12
        //paths[VivaldiSkybridgePath.AllianceColor.BLUE.ordinal()][VivaldiSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 12, -0.4); //forward to latch, 12
       // paths[VivaldiSkybridgePath.AllianceColor.BLUE.ordinal()][VivaldiSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 12, 0.2); //forward to latch, 12
       // paths[VivaldiSkybridgePath.AllianceColor.BLUE.ordinal()][VivaldiSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 20, 0.4); //forward to latch, 12
      //  paths[VivaldiSkybridgePath.AllianceColor.BLUE.ordinal()][VivaldiSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 12, -0.4); //forward to latch, 12
      //  paths[VivaldiSkybridgePath.AllianceColor.BLUE.ordinal()][VivaldiSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 6, 0.4); //forward to latch, 12
      //  paths[VivaldiSkybridgePath.AllianceColor.BLUE.ordinal()][VivaldiSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 6, 0.4); //forward to latch, 12

        // BLUE ALLIANCE, LOADING SIDE
        paths[VivaldiSkybridgePath.AllianceColor.BLUE.ordinal()][VivaldiSkybridgePath.StartingPosition.LOADING.ordinal()] = new DeadReckonPath();
        paths[VivaldiSkybridgePath.AllianceColor.BLUE.ordinal()][VivaldiSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 12, 0.2); //forward, 20
        paths[VivaldiSkybridgePath.AllianceColor.BLUE.ordinal()][VivaldiSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 30, -0.8); //left, 20

        // paths[VivaldiSkybridgePath.AllianceColor.BLUE.ordinal()][VivaldiSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, -62, 0.2); //left to latch, -62
        //paths[VivaldiSkybridgePath.AllianceColor.BLUE.ordinal()][VivaldiSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, -30, 0.2); //backward, -30

        // RED ALLIANCE, BUILDING SIDE
        paths[VivaldiSkybridgePath.AllianceColor.RED.ordinal()][VivaldiSkybridgePath.StartingPosition.BUILDING.ordinal()] = new DeadReckonPath();
        paths[VivaldiSkybridgePath.AllianceColor.RED.ordinal()][VivaldiSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 20, 0.4); //forward to latch, 20
        //latch to foundation
        paths[VivaldiSkybridgePath.AllianceColor.RED.ordinal()][VivaldiSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 12, 0.4); //sideways left to latch, 12
        paths[VivaldiSkybridgePath.AllianceColor.RED.ordinal()][VivaldiSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 12, -0.8); //backwards with foundation, 12
        paths[VivaldiSkybridgePath.AllianceColor.RED.ordinal()][VivaldiSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 90, 0.2); //turning foundation, 12
        paths[VivaldiSkybridgePath.AllianceColor.RED.ordinal()][VivaldiSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 20, 0.4); //forward to move foundation, 20
        paths[VivaldiSkybridgePath.AllianceColor.RED.ordinal()][VivaldiSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 12, -0.4); //backward to park, 12
        paths[VivaldiSkybridgePath.AllianceColor.RED.ordinal()][VivaldiSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, -180, 0.4); //turn right to park, 12
        paths[VivaldiSkybridgePath.AllianceColor.RED.ordinal()][VivaldiSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 12, -0.4); //sideways right to park, 12

        //paths[VivaldiSkybridgePath.AllianceColor.BLUE.ordinal()][VivaldiSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, -30, 0.2); //backward, -30

        // RED ALLIANCE, LOADING SIDE
        paths[VivaldiSkybridgePath.AllianceColor.RED.ordinal()][VivaldiSkybridgePath.StartingPosition.LOADING.ordinal()] = new DeadReckonPath();
        paths[VivaldiSkybridgePath.AllianceColor.RED.ordinal()][VivaldiSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 12, 0.2); //forward, 12
        paths[VivaldiSkybridgePath.AllianceColor.RED.ordinal()][VivaldiSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 30, 0.8); //right, 10

        //paths[VivaldiSkybridgePath.AllianceColor.BLUE.ordinal()][VivaldiSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, -30, 0.2); //backward, -30
    }

    public DeadReckonPath getPath(VivaldiSkybridgePath.AllianceColor allianceColor, VivaldiSkybridgePath.StartingPosition startingPosition) {
        return paths[allianceColor.ordinal()][startingPosition.ordinal()];
    }
}
