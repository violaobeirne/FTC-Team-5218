package opmodes.LM0;

import com.qualcomm.robotcore.eventloop.opmode.TeleOp;
import com.qualcomm.robotcore.hardware.DcMotor;
import com.qualcomm.robotcore.hardware.Servo;
import opmodes.HisaishiCalibration;
import team25core.FourWheelDirectDrivetrain;
import team25core.GamepadTask;
import team25core.Robot;
import team25core.RobotEvent;
import team25core.TankDriveTask;

@TeleOp(name = "Foundation Calibration Test")
//@Disabled
public class FoundationMoverNewCalibration extends Robot {

    private DcMotor frontLeft;
    private DcMotor frontRight;
    private DcMotor backLeft;
    private DcMotor backRight;

    private Servo leftArm;

    private FourWheelDirectDrivetrain drivetrain;

    @Override
    public void handleEvent(RobotEvent e)
    {
        if (e instanceof GamepadTask.GamepadEvent) {
            GamepadTask.GamepadEvent event = (GamepadTask.GamepadEvent) e;

            /*switch (event.kind) {
                case BUTTON_X_DOWN:
                    leftArm.setPosition(HisaishiCalibration.ARM_LEFT_DOWN);
                    //rightArm.setPosition(HisaishiCalibration.ARM_RIGHT_DOWN);
                    break;

                case BUTTON_B_DOWN:
                    leftArm.setPosition(HisaishiCalibration.ARM_LEFT_STOW);
                    //rightArm.setPosition(HisaishiCalibration.ARM_RIGHT_STOW);
                    break;
                    */
            }
        }


    @Override
    public void init()
    {
        frontLeft = hardwareMap.get(DcMotor.class, "frontLeft");
        frontRight = hardwareMap.get(DcMotor.class, "frontRight");
        backLeft = hardwareMap.get(DcMotor.class, "backLeft");
        backRight = hardwareMap.get(DcMotor.class, "backRight");

        //leftArm = hardwareMap.servo.get("leftArm");

        GamepadTask gamepad= new GamepadTask(this, GamepadTask.GamepadNumber.GAMEPAD_1);
        addTask(gamepad);

        drivetrain = new FourWheelDirectDrivetrain(frontRight, backRight, frontLeft, backLeft);
    }


    @Override
    public void start()
    {
        this.addTask(new TankDriveTask(this, drivetrain));
    }

}
