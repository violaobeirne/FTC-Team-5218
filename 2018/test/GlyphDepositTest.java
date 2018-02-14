package test;

import com.qualcomm.robotcore.eventloop.opmode.Autonomous;
import com.qualcomm.robotcore.eventloop.opmode.Disabled;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.DcMotorSimple;
import com.qualcomm.robotcore.hardware.Servo;

import org.firstinspires.ftc.robotcore.external.Telemetry;

import opmodes.GlyphDeposits;
import opmodes.HisaishiCalibration;
import team25core.DeadReckonPath;
import team25core.DeadReckonTask;
import team25core.Drivetrain;
import team25core.FourWheelDirectDrivetrain;
import team25core.GamepadTask;
import team25core.Robot;
import team25core.RobotEvent;

import static team25core.GamepadTask.EventKind.BUTTON_B_DOWN;

/**
 * Created by Lizzie on 12/23/2017.
 */
@Autonomous(name = "Glyph Deposit Test")
@Disabled
public class GlyphDepositTest extends Robot {

    private DcMotor frontLeft;
    private DcMotor frontRight;
    private DcMotor backLeft;
    private DcMotor backRight;
    private Servo glyphRGrabber;
    private Servo glyphLGrabber;
    private Servo jewelArm;
    private FourWheelDirectDrivetrain drivetrain;

    private GlyphDeposits glyphDeposits;
    private DeadReckonPath glyphPath;
    private DeadReckonTask task;

    private Telemetry.Item startingPosition;
    private Telemetry.Item column;

    @Override
    public void init() {
        frontLeft = hardwareMap.dcMotor.get("frontL");
        frontRight = hardwareMap.dcMotor.get("frontR");
        backLeft = hardwareMap.dcMotor.get("backL");
        backRight = hardwareMap.dcMotor.get("backR");
        glyphLGrabber = hardwareMap.servo.get("glyphLeftGrabber");
        glyphRGrabber = hardwareMap.servo.get("glyphRightGrabber");
        jewelArm = hardwareMap.servo.get("jewelYAxis");

        drivetrain = new FourWheelDirectDrivetrain(frontRight, backRight, frontLeft, backLeft);

        drivetrain.resetEncoders();
        drivetrain.encodersOn();
        drivetrain.setMasterMotor(backLeft);


        startingPosition = telemetry.addData("Starting Position: ", "NOT SELECTED");
        column = telemetry.addData("Column: ", "NOT SELECTED");

        glyphDeposits = new GlyphDeposits();



        this.addTask(new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_1) {
            public void handleEvent(RobotEvent e) {
                GamepadEvent event = (GamepadEvent) e;
                switch (event.kind) {
                    case BUTTON_X_DOWN:
                        glyphPath = glyphDeposits.getPath(GlyphDeposits.StartingPosition.R1, GlyphDeposits.TargetColumn.LEFT);
                        startingPosition.setValue("R1");
                        column.setValue("LEFT");
                        break;
                    case BUTTON_Y_DOWN:
                        glyphPath = glyphDeposits.getPath(GlyphDeposits.StartingPosition.R1, GlyphDeposits.TargetColumn.MIDDLE);
                        startingPosition.setValue("R1");
                        column.setValue("MIDDLE");
                        break;
                    case BUTTON_A_DOWN:
                        glyphPath = glyphDeposits.getPath(GlyphDeposits.StartingPosition.R1, GlyphDeposits.TargetColumn.RIGHT);
                        startingPosition.setValue("R1");
                        column.setValue("RIGHT");
                        break;
                    case BUTTON_B_DOWN:
                        glyphPath = glyphDeposits.getPath(GlyphDeposits.StartingPosition.R2, GlyphDeposits.TargetColumn.LEFT);
                        startingPosition.setValue("R2");
                        column.setValue("LEFT");
                        break;
                    case RIGHT_BUMPER_DOWN:
                        glyphPath = glyphDeposits.getPath(GlyphDeposits.StartingPosition.R2, GlyphDeposits.TargetColumn.MIDDLE);
                        startingPosition.setValue("R2");
                        column.setValue("MIDDLE");
                        break;
                    case RIGHT_TRIGGER_DOWN:
                        glyphPath = glyphDeposits.getPath(GlyphDeposits.StartingPosition.R2, GlyphDeposits.TargetColumn.RIGHT);
                        startingPosition.setValue("R2");
                        column.setValue("RIGHT");
                        break;
                    case LEFT_BUMPER_DOWN:
                        glyphPath = glyphDeposits.getPath(GlyphDeposits.StartingPosition.B1, GlyphDeposits.TargetColumn.LEFT);
                        startingPosition.setValue("B1");
                        column.setValue("LEFT");
                        break;
                    case LEFT_TRIGGER_DOWN:
                        glyphPath = glyphDeposits.getPath(GlyphDeposits.StartingPosition.B1, GlyphDeposits.TargetColumn.MIDDLE);
                        startingPosition.setValue("B1");
                        column.setValue("MIDDLE");
                        break;
                    default:
                        break;
                }
            }
        });

        this.addTask(new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_2) {
            public void handleEvent(RobotEvent e) {
                GamepadEvent event = (GamepadEvent) e;
                switch (event.kind) {
                    case BUTTON_A_DOWN:
                        glyphPath = glyphDeposits.getPath(GlyphDeposits.StartingPosition.B1, GlyphDeposits.TargetColumn.RIGHT);
                        startingPosition.setValue("B1");
                        column.setValue("RIGHT");
                        break;
                    case BUTTON_B_DOWN:
                        glyphPath = glyphDeposits.getPath(GlyphDeposits.StartingPosition.B2, GlyphDeposits.TargetColumn.LEFT);
                        startingPosition.setValue("B2");
                        column.setValue("LEFT");
                        break;
                    case BUTTON_X_DOWN:
                        glyphPath = glyphDeposits.getPath(GlyphDeposits.StartingPosition.B2, GlyphDeposits.TargetColumn.MIDDLE);
                        startingPosition.setValue("B2");
                        column.setValue("MIDDLE");
                        break;
                    case BUTTON_Y_DOWN:
                        glyphPath = glyphDeposits.getPath(GlyphDeposits.StartingPosition.B2, GlyphDeposits.TargetColumn.RIGHT);
                        startingPosition.setValue("B2");
                        column.setValue("RIGHT");
                        break;
                    default:
                        break;
                }
            }
        });
    }

    @Override
    public void handleEvent(RobotEvent e)
    {

    }

    @Override
    public void start()
    {
        task = new DeadReckonTask(this, glyphPath, drivetrain);
        glyphRGrabber.setPosition(HisaishiCalibration.GLYPH_CLOSE_RIGHT_POSITION);
        glyphLGrabber.setPosition(HisaishiCalibration.GLYPH_CLOSE_LEFT_POSITION);
        jewelArm.setPosition(HisaishiCalibration.JEWEL_Y_AXIS_STOWED);
        addTask(task);
    }

}
