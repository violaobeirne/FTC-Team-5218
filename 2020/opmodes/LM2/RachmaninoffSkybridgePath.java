package opmodes.LM2;

import team25core.DeadReckonPath;

/**
 * Created by Lizzie on 11/5/2019.
 */
public class RachmaninoffSkybridgePath {
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

    public RachmaninoffSkybridgePath() {
        // BLUE ALLIANCE, BUILDING SIDE
        paths[RachmaninoffSkybridgePath.AllianceColor.BLUE.ordinal()][RachmaninoffSkybridgePath.StartingPosition.BUILDING.ordinal()] = new DeadReckonPath();
        paths[RachmaninoffSkybridgePath.AllianceColor.BLUE.ordinal()][RachmaninoffSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 22, 0.8); //forward to latch, 30
        paths[RachmaninoffSkybridgePath.AllianceColor.BLUE.ordinal()][RachmaninoffSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 33.25, -0.6); //forward to latch, 30
        // paths[RachmaninoffSkybridgePath.AllianceColor.BLUE.ordinal()][RachmaninoffSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 40, 0.4); //forward to latch, 30

        // paths[RachmaninoffSkybridgePath.AllianceColor.BLUE.ordinal()][RachmaninoffSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 22, -0.8); //forward to latch, 30
        // paths[RachmaninoffSkybridgePath.AllianceColor.BLUE.ordinal()][RachmaninoffSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 33.25, -0.6); //right, -35.25
        // paths[RachmaninoffSkybridgePath.AllianceColor.BLUE.ordinal()][RachmaninoffSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 3.25, -0.6); //right, -35.25
        // paths[RachmaninoffSkybridgePath.AllianceColor.BLUE.ordinal()][RachmaninoffSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 8, -0.2); //right, -35.25
        // paths[RachmaninoffSkybridgePath.AllianceColor.BLUE.ordinal()][RachmaninoffSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 35, 0.4); //right, -35.25

        // BLUE ALLIANCE, LOADING SIDE
        paths[RachmaninoffSkybridgePath.AllianceColor.BLUE.ordinal()][RachmaninoffSkybridgePath.StartingPosition.LOADING.ordinal()] = new DeadReckonPath();
        paths[RachmaninoffSkybridgePath.AllianceColor.BLUE.ordinal()][RachmaninoffSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 13, 0.2); //forward, 20
        paths[RachmaninoffSkybridgePath.AllianceColor.BLUE.ordinal()][RachmaninoffSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 8, 0.2); //forward, 20
        // paths[RachmaninoffSkybridgePath.AllianceColor.BLUE.ordinal()][RachmaninoffSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, -62, 0.2); //left to latch, -62
        //paths[RachmaninoffSkybridgePath.AllianceColor.BLUE.ordinal()][RachmaninoffSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, -30, 0.2); //backward, -30

        // RED ALLIANCE, BUILDING SIDE
        paths[RachmaninoffSkybridgePath.AllianceColor.RED.ordinal()][RachmaninoffSkybridgePath.StartingPosition.BUILDING.ordinal()] = new DeadReckonPath();
        paths[RachmaninoffSkybridgePath.AllianceColor.RED.ordinal()][RachmaninoffSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 30, 0.2); //forward, 30
        paths[RachmaninoffSkybridgePath.AllianceColor.RED.ordinal()][RachmaninoffSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.SIDEWAYS, 35.25, -0.2); //left, 35.25
        //paths[RachmaninoffSkybridgePath.AllianceColor.BLUE.ordinal()][RachmaninoffSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, -30, 0.2); //backward, -30

        // RED ALLIANCE, LOADING SIDE
        paths[RachmaninoffSkybridgePath.AllianceColor.RED.ordinal()][RachmaninoffSkybridgePath.StartingPosition.LOADING.ordinal()] = new DeadReckonPath();
        paths[RachmaninoffSkybridgePath.AllianceColor.RED.ordinal()][RachmaninoffSkybridgePath.StartingPosition.LOADING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, 10, 0.2); //forward, 20
        //paths[RachmaninoffSkybridgePath.AllianceColor.BLUE.ordinal()][RachmaninoffSkybridgePath.StartingPosition.BUILDING.ordinal()].addSegment(DeadReckonPath.SegmentType.STRAIGHT, -30, 0.2); //backward, -30
    }

    public DeadReckonPath getPath(RachmaninoffSkybridgePath.AllianceColor allianceColor, RachmaninoffSkybridgePath.StartingPosition startingPosition) {
        return paths[allianceColor.ordinal()][startingPosition.ordinal()];
    }
}
