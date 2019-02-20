package opmodes.Reggie;

import org.firstinspires.ftc.robotcore.external.Telemetry;
import org.firstinspires.ftc.robotcore.external.tfod.Recognition;

import java.util.List;

import team25core.DeadReckonPath;

import static opmodes.Reggie.ReggieDropoffUtil.MineralPosition.UNKNOWN;
import static team25core.MineralDetectionTask.LABEL_GOLD_MINERAL;

/**
 * Created by Lizzie on 2/6/2019.
 */
public class ReggieDropoffUtil {

    // groundwork for autonomous variables
    public enum StartingPosition {
        CRATER,
        DEPOT,
        DEFAULT,
    }

    public enum EndingPosition {
        PARKING,
        NOT_PARKING,
        DOUBLE_SAMPLING,
        DEFAULT,
    }

    public enum MineralPosition {
        LEFT,
        RIGHT,
        CENTER,
        UNKNOWN,
    }

    public static void sendPositionTelemetry(MineralPosition pos, Telemetry.Item item) {
        switch (pos) {
            case LEFT:
                item.setValue("LEFT");
                break;
            case RIGHT:
                item.setValue("RIGHT");
                break;
            case CENTER:
                item.setValue("CENTER");
                break;
        }
    }

    public static MineralPosition determineGoldPosition(List<Recognition> minerals) {
        if (minerals != null) {
            int goldMineralX = -1;
            int silverMineral1X = -1;
            int silverMineral2X = -1;
            for (Recognition recognition : minerals) {
                if (recognition.getLabel().equals(LABEL_GOLD_MINERAL)) {
                    goldMineralX = (int) recognition.getLeft();
                } else if (silverMineral1X == -1) {
                    silverMineral1X = (int) recognition.getLeft();
                } else {
                    silverMineral2X = (int) recognition.getLeft();
                }
            }

            // two mineral detection code
            if (goldMineralX != -1 && silverMineral1X != -1) {
                if (goldMineralX < silverMineral1X) {
                    return MineralPosition.CENTER;
                } else if (goldMineralX > silverMineral1X) {
                    return MineralPosition.RIGHT;
                }
            } else if (silverMineral1X != -1 && silverMineral2X != -1) {
                return MineralPosition.LEFT;
            }
            /*
            // three mineral detection code
            if (goldMineralX != -1 && silverMineral1X != -1 && silverMineral2X != -1) {
                if (goldMineralX < silverMineral1X && goldMineralX < silverMineral2X) {
                    return MineralPosition.LEFT;
                } else if (goldMineralX > silverMineral1X && goldMineralX > silverMineral2X) {
                    return MineralPosition.RIGHT;
                } else {
                    return MineralPosition.CENTER;
                }
            }
            */

        }
        return UNKNOWN;
    }

    DeadReckonPath[][][] paths = new DeadReckonPath[2][3][3];

    public ReggieDropoffUtil () {
        /*
        CRATER CODE: double sampling OR parking in 3 paths
        */

        // crater, double sampling, center mineral
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.DOUBLE_SAMPLING.ordinal()][MineralPosition.CENTER.ordinal()] = new DeadReckonPath();

        // crater, parking, center
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.CENTER.ordinal()] = new DeadReckonPath();

        // crater, double sampling, left mineral path
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.DOUBLE_SAMPLING.ordinal()][MineralPosition.LEFT.ordinal()] = new DeadReckonPath();

        // crater, parking, left mineral path
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.LEFT.ordinal()] = new DeadReckonPath();

        // crater, double sampling, right mineral path
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.DOUBLE_SAMPLING.ordinal()][MineralPosition.RIGHT.ordinal()] = new DeadReckonPath();

        // crater, parking, right mineral path
        paths[StartingPosition.CRATER.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.RIGHT.ordinal()] = new DeadReckonPath();

        /*
        DEPOT CODE: marker dropoff with parking
        */

        // depot, parking, center mineral
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.CENTER.ordinal()] = new DeadReckonPath();

        // depot, no parking, center mineral
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.NOT_PARKING.ordinal()][MineralPosition.CENTER.ordinal()] = new DeadReckonPath();

        // depot, parking, left mineral
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.LEFT.ordinal()] = new DeadReckonPath();

        // parking portion
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.LEFT.ordinal()].addSegment(DeadReckonPath.SegmentType.TURN, 9.0, 0.5);

        // depot, no parking, left mineral
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.NOT_PARKING.ordinal()][MineralPosition.LEFT.ordinal()] = new DeadReckonPath();

        // depot, parking, right mineral
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.PARKING.ordinal()][MineralPosition.RIGHT.ordinal()] = new DeadReckonPath();

        // depot, no parking, right mineral
        paths[StartingPosition.DEPOT.ordinal()][EndingPosition.NOT_PARKING.ordinal()][MineralPosition.RIGHT.ordinal()] = new DeadReckonPath();
    }

    public DeadReckonPath getPath (ReggieDropoffUtil.StartingPosition start, ReggieDropoffUtil.EndingPosition end, ReggieDropoffUtil.MineralPosition position)
    {
        return paths[start.ordinal()][end.ordinal()][position.ordinal()];
    }
}
