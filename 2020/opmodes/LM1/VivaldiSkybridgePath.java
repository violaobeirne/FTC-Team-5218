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
        paths[VivaldiSkybridgePath.AllianceColor.BLUE.ordinal()][VivaldiSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 22, 0.8); //forward to latch, 30
        paths[VivaldiSkybridgePath.AllianceColor.BLUE.ordinal()][VivaldiSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 33.25, -0.6); //forward to latch, 30
        // paths[VivaldiSkybridgePath.AllianceColor.BLUE.ordinal()][VivaldiSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 40, 0.4); //forward to latch, 30

        // paths[VivaldiSkybridgePath.AllianceColor.BLUE.ordinal()][VivaldiSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 22, -0.8); //forward to latch, 30
        // paths[VivaldiSkybridgePath.AllianceColor.BLUE.ordinal()][VivaldiSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 33.25, -0.6); //right, -35.25
        // paths[VivaldiSkybridgePath.AllianceColor.BLUE.ordinal()][VivaldiSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 3.25, -0.6); //right, -35.25
        // paths[VivaldiSkybridgePath.AllianceColor.BLUE.ordinal()][VivaldiSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 8, -0.2); //right, -35.25
        // paths[VivaldiSkybridgePath.AllianceColor.BLUE.ordinal()][VivaldiSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 35, 0.4); //right, -35.25

        // BLUE ALLIANCE, LOADING SIDE
        paths[VivaldiSkybridgePath.AllianceColor.BLUE.ordinal()][VivaldiSkybridgePath.StartingPosition.LOADING.ordinal()] = new DeadReckonPath();
        paths[VivaldiSkybridgePath.AllianceColor.BLUE.ordinal()][VivaldiSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 13, 0.2); //forward, 20
        paths[VivaldiSkybridgePath.AllianceColor.BLUE.ordinal()][VivaldiSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 8, 0.2); //forward, 20
        // paths[VivaldiSkybridgePath.AllianceColor.BLUE.ordinal()][VivaldiSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, -62, 0.2); //left to latch, -62
        //paths[VivaldiSkybridgePath.AllianceColor.BLUE.ordinal()][VivaldiSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, -30, 0.2); //backward, -30

        // RED ALLIANCE, BUILDING SIDE
        paths[VivaldiSkybridgePath.AllianceColor.RED.ordinal()][VivaldiSkybridgePath.StartingPosition.BUILDING.ordinal()] = new DeadReckonPath();
        paths[VivaldiSkybridgePath.AllianceColor.RED.ordinal()][VivaldiSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 30, 0.2); //forward, 30
        paths[VivaldiSkybridgePath.AllianceColor.RED.ordinal()][VivaldiSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 35.25, -0.2); //left, 35.25
        //paths[VivaldiSkybridgePath.AllianceColor.BLUE.ordinal()][VivaldiSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, -30, 0.2); //backward, -30

        // RED ALLIANCE, LOADING SIDE
        paths[VivaldiSkybridgePath.AllianceColor.RED.ordinal()][VivaldiSkybridgePath.StartingPosition.LOADING.ordinal()] = new DeadReckonPath();
        paths[VivaldiSkybridgePath.AllianceColor.RED.ordinal()][VivaldiSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 10, 0.2); //forward, 20
        //paths[VivaldiSkybridgePath.AllianceColor.BLUE.ordinal()][VivaldiSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, -30, 0.2); //backward, -30
    }

    public DeadReckonPath getPath(VivaldiSkybridgePath.AllianceColor allianceColor, VivaldiSkybridgePath.StartingPosition startingPosition) {
        return paths[allianceColor.ordinal()][startingPosition.ordinal()];
    }
}
